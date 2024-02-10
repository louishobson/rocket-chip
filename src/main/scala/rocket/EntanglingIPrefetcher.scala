package freechips.rocketchip.rocket

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.conversions._
import chisel3.util.{Cat, Decoupled, Enum, Mux1H, OHToUInt, PopCount, Queue, RegEnable, ShiftRegister, UIntToOH, Valid, isPow2, log2Up, switch, is}
import chisel3.util.random.LFSR
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.unittest._
import freechips.rocketchip.util.{DescribedSRAM, PartitionsOH, Split}
import org.chipsalliance.cde.config.Parameters



/** [[EntanglingIPrefetcherConfig]] defines configuration variables for the prefetcher.
  */
case class EntanglingIPrefetcherParams(
  /* The bit width of timestamps */
  timeBits: Int = 64,
  /* the maximum size of a BB */
  maxBBSize: Int = 63,
  /* The minimum size of a 'significant' BB */
  sigBBSize: Int = 2,
  /* The maximum forward-jumping gap in baddrs that should still be considered the same basic block */
  maxBBGapSize: Int = 2, 
  /* The length of the history buffer */
  histBufLen: Int = 16,
  /* The number of elements of the history buffer to search in one combinatorial path */
  histBufSearchFragLen: Int = 4,
  /* The ways and sets of the entangling table */
  nWays: Int = 4,
  nSets: Int = 1024,
  /* The queue sizes for entangling table IO */
  eTablePrefetchQueueSize: Int = 8,
  eTableUpdateQueueSize: Int = 4,
  eTableEntangleQueueSize: Int = 2,
  /* Queue size for outputting prefetches */
  prefetchQueueSize: Int = 8,
  /* The number of outstanding prefetch requests */
  nPrefetchMSHRs: Int = 3,
  /* The number of bits used to store entanglings */
  entanglingBits: Int = 63,
  /* The maximum number of entanglings which can be stored in entanglingBits */
  maxEntanglings: Int = 6
)

/** [[HasEntanglingIPrefetcherParameters]] is the trait for a class which needs EntanglingIPrefetcherParams
  */ 
trait HasEntanglingIPrefetcherParameters extends HasL1ICacheParameters {
  /* The parameters structure */
  implicit val p: Parameters
  val entanglingParams: EntanglingIPrefetcherParams = cacheParams.entanglingParams.get

  /* Copy out the parameters */
  def timeBits = entanglingParams.timeBits
  def maxBBSize = entanglingParams.maxBBSize
  def histBufLen = entanglingParams.histBufLen
  def sigBBSize = entanglingParams.sigBBSize
  def maxBBGapSize = entanglingParams.maxBBGapSize
  def histBufSearchFragLen = entanglingParams.histBufSearchFragLen
  def eTableNWays = entanglingParams.nWays
  def eTableNSets = entanglingParams.nSets
  def eTablePrefetchQueueSize = entanglingParams.eTablePrefetchQueueSize
  def eTableUpdateQueueSize = entanglingParams.eTableUpdateQueueSize
  def eTableEntangleQueueSize = entanglingParams.eTableEntangleQueueSize
  def prefetchQueueSize = entanglingParams.prefetchQueueSize
  def nPrefetchMSHRs = entanglingParams.nPrefetchMSHRs
  def entanglingBits = entanglingParams.entanglingBits
  def maxEntanglings = entanglingParams.maxEntanglings

  /* The block address size */
  def baddrBits = paddrBits - blockOffBits

  /* We may need to store some bits to re-create the virtual address enough to produce the I$ index.
   *   |--------|--pgIdxBits--|
   *           blockOffBits ¬
   *   |----|---idxBits---|---|
   * vidxBits ¬
   *   |----|---|-------------|
   */
  def vidxBits = 0.max(blockOffBits + idxBits - pgIdxBits)

  /* Configuration for the entangling table */
  require(isPow2(nWays), "entanglingNSets must be a power of 2")
  def entIdxBits = log2Up(nSets)
  def entTagBits = baddrBits - entIdxBits

  /* The number of bits required to store a BB size */
  def lgMaxBBSize = log2Up(maxBBSize + 1).toInt

  /* The maximum history buffer search latency */
  def maxHistBufSearchLatency = math.ceil(histBufLen/histBufSearchFragLen).toInt + 1

  /* The maximum possible baddr and time */
  def maxTime = (math.pow(2, timeBits)-1).toInt
  def maxBaddr = (math.pow(2, baddrBits)-1).toInt

  /* The bits required to store the number of entanglings */
  def entanglingSizeBits = log2Up(maxEntanglings+1)

  /* The bits used to just store the entangling addresses.
   * Assert that at least one address can be fully stored.
   */
  def entanglingAddrBits = entanglingBits - entanglingSizeBits
  assert(entanglingAddrBits >= baddrBits)

  /* The sequence of masks where the set bits are the bits lost by compression */
  def entanglingEqualityMaskLookup = Seq.tabulate(maxEntanglings+2){case 0 => 0; case i =>
    val entryBits = (entanglingAddrBits/i) min baddrBits
    val compressBits = baddrBits - entryBits
    ((1<<compressBits)-1)<<entryBits
  }
}



/** [[EntanglerEncodeReq]] A request to encode, or response to decode for entangling operations.
  */ 
class EntanglerEncodeReq(nBaddrs: Int)(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val baddrs = Vec(nBaddrs, UInt(baddrBits.W))
  val len = UInt(log2Up(nBaddrs+1).W)
}

/** [[EntanglerEncodeResp]] A response to encode, or a request to decode for entangling operations.
  */ 
class EntanglerEncodeResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val ents = Bits(entanglingBits.W)
}

/** [[EntanglingEncoder]] compresses baddrs into a single entanglingBits-bit value.
  * The process can take a varying number of cycles depending on how many addresses need to be dropped.
  * For this reason, the IO is decoupled. 
  *
  * @param keepFirstBaddr Whether the first baddr in the input sequence should be dropped by random replacement.
  * This is useful when a newly-added baddr is always the first in the input sequence. 
  */
class EntanglingEncoder(keepFirstBaddr: Boolean = true)(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO. The encoding process can take multiple cycles, so use decoupled IO. */
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new EntanglerEncodeReq(maxEntanglings+1)))
    val resp = Valid(new EntanglerEncodeResp)
  })

  /* Create registers for each of the inputs */
  val req = Reg(new EntanglerEncodeReq(maxEntanglings+1))

  /* Create a register for whether we are busy or not */
  val busy = RegInit(false.B)

  /* We want to inform the user whether we are busy or not */
  io.req.ready := !busy

  /* Set the default output ports */
  io.resp.valid := false.B
  io.resp.bits.head := req.head
  io.resp.bits.ents := DontCare

  /* If we are ready and there is available input, then consume it */
  when(io.req.fire) {
    /* Save the input and move to the ready state */
    busy := true.B
    req := io.req.bits

    /* Check that we haven't been given more than (maxEntanglings+1) baddrs */
    assert(io.req.bits.len <= (maxEntanglings+1).U)
  }

  /* If we are in the busy state, then continue with the ongoing encoding */
  when(busy) {
    /* The number of bits each baddr will be compressed to.
     * This is equivalent to (entanglingAddrBits.U / req.len), but as a lookup table.
     */
    val entanglingMask = VecInit(entanglingEqualityMaskLookup.map(_.U(baddrBits.W)))(req.len)

    /* Consider whether we can perform the compression. We must have
     *  - maxEntanglings or fewer addresses to entangle, and
     *  - each address must share the same MSBs as the head.
     */
    val baddrs_okay = req.len =/= (maxEntanglings+1).U && req.baddrs.zipWithIndex.map{
      case (baddr, i) => (baddr & entanglingMask) === (req.head & entanglingMask) || i.U >= req.len
    }.reduce(_&&_)

    /* We can produce an output entangling sequence when baddrs_okay is flagged.
     * Note that io.resp.bits.ents is initialized to 0, which is the correct output for the case where req.len is 0.
     */
    when(baddrs_okay) {
      /* Encoding is finished and the output is now valid */
      busy := false.B
      io.resp.valid := true.B
      /* Generate hardware for each possible number of entanglings and then select the correct output.
       * The hardware just consists of splicing the correct bits together in the right positions, 
       * which would be quite problematic to do dynamically.
       */
      for(i <- 1 to maxEntanglings) {
        when(i.U === req.len) {
          io.resp.bits.ents := 
            req.len(entanglingSizeBits-1,0) ## 
            0.U((entanglingAddrBits%i).W) ## 
            req.baddrs.take(i).map(_.pad(entanglingAddrBits/i)(entanglingAddrBits/i-1,0)).reduce(_##_)
        }
      }
    } 
    
    /* Otherwise (baddrs_okay is false) we need to randomly pop one of the addresses */
    .otherwise {
      /* There should be at least two baddrs left */
      assert(req.len > 1.U)

      /* Generate a OH value indicating which baddr is the victim */
      val maxPossibleVictims = if (keepFirstBaddr) maxEntanglings else maxEntanglings+1
      val req_len1 = req.len-1.U
      val req_len2 = req.len-2.U
      val rnd_oh = VecInit(PartitionsOH(LFSR(8, busy && !baddrs_okay), maxPossibleVictims))(
        if (keepFirstBaddr) req_len2 else req_len1
      ).pad(maxPossibleVictims)

      /* Move the last register to the position of the evictee */
      for(i <- 0 until maxPossibleVictims) {
        when(rnd_oh(i)) {
          req.baddrs(if (keepFirstBaddr) i+1 else i) := req.baddrs(req_len1)
        }
      }
      
      /* Decrement the length register */
      req.len := req_len1
    }
  }  
}

/** [[EntanglingDecoder]] Decompresses compressed baddrs into separate registers and a length indicator.
  */
class EntanglingDecoder(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO. We don't need Valid IO as the process is entirely combinatorial. */
  val io = IO(new Bundle {
    val req = Input(new EntanglerEncodeResp)
    val resp = Output(new EntanglerEncodeReq(maxEntanglings))
  })

  /* Get the mode of the entangling */
  val mode = io.req.ents(entanglingBits-1,entanglingAddrBits)

  /* Set the the output. We will overwrite baddrs. */
  io.resp.head := io.req.head
  io.resp.baddrs := DontCare
  io.resp.len := mode

  /* Generate hardware for decoding each possible number entanglings and select the correct output.
   * Note that we don't care about the output vector when there zero entanglings.
   */
  for (i <- 1 to maxEntanglings) {
    when(i.U === mode) {
      /* This is the number of bits for each entangling entry in this mode */
      val entryBits = entanglingAddrBits / i

      /* Iterate over the number of addresses we need to extract */
      for (j <- 0 until i) {
        /* The logic is simpler when the addresses are not compressed */
        if (entryBits >= baddrBits) {
          io.resp.baddrs(i-j-1) := io.req.ents(baddrBits+j*entryBits-1,j*entryBits)
        /* Otherwise we need to reconstruct the address based on the head */
        } else {
          io.resp.baddrs(i-j-1) := io.req.head(baddrBits-1,entryBits) ## io.req.ents((j+1)*entryBits-1,j*entryBits)
        }
      }
    }
  }
}



/** [[BBCounterReq]] defines the interface for notifying the BB counter of a new baddr being fetched.
  */ 
class BBCounterReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val baddr = UInt(baddrBits.W)
  val vidx = UInt(vidxBits.W)
  val time = UInt(timeBits.W)
}

/** [[BBCounterSearchReq]] defines the interface for when a BB is completed.
  */
class BBCounterResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val vidx = UInt(vidxBits.W)
  val size = UInt(lgMaxBBSize.W)
  val time = UInt(timeBits.W)
  val done = Bool()
}

/** [[BBCounter]] implements a counter for baddrs, emitting a when appropriate.
  */
class BBCounter(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {
  
  /* Define the IO */
  val io = IO(new Bundle {
    val req = Flipped(Valid(new BBCounterReq))
    val resp = Output(new BBCounterResp)
  })

  /* Initialize the BB head, size and timestamp registers */
  val bb_head = Reg(UInt(baddrBits.W))
  val bb_vidx = Reg(UInt(vidxBits.W))
  val bb_size = RegInit(0.U(lgMaxBBSize.W))
  val bb_time = Reg(UInt(timeBits.W))
  
  /* Calculate the change in io.req.bits.baddr compared to the current head.
   * We need to be quite careful regarding overflow while performing these calculations.
   * cont indicates that we are within the same BB (true if baddr is not valid).
   * grow indicates that we have just extended the BB size (false if baddr is not valid).
   */
  val req_bdiff = io.req.bits.baddr - bb_head
  val cont = !io.req.valid || (io.req.bits.baddr >= bb_head && req_bdiff <= bb_size +& maxBBGapSize.U && req_bdiff < maxBBSize.U && io.req.bits.vidx === bb_vidx)
  val grow = io.req.valid && cont && req_bdiff >= bb_size

  /* Update the registers */
  when(!cont || (bb_size === 0.U && io.req.valid)) {
    bb_head := io.req.bits.baddr
    bb_vidx := io.req.bits.vidx
    bb_time := io.req.bits.time 
    bb_size := 1.U
  } .elsewhen(grow) {
    bb_size := req_bdiff + 1.U
  }

  /* Potentially output a completed significant BB. This is when a basic block reaches a size of sigBBSize.
   * This can stop tiny basic blocks from filling the entangling table.
   */
  io.resp.done := !cont && bb_size >= sigBBSize.U
  io.resp.head := bb_head
  io.resp.vidx := bb_vidx
  io.resp.size := bb_size
  io.resp.time := bb_time

}



/** [[HistoryBufferInsertReq]] defines the interface for requesting an insert operation on the HB.
  */ 
class HistoryBufferInsertReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val time = UInt(timeBits.W)
}

/** [[HistoryBufferSearchReq]] defines the interface for requesting a search of the HB.
  */
class HistoryBufferSearchReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val dst = UInt(baddrBits.W)
  val target_time = UInt(timeBits.W)
}

/** [[HistoryBufferSearchResp]] defines the interface for responding to a HB search request.
  * If the search is unsuccessful, then no response is made. 
  */
class HistoryBufferSearchResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val src = UInt(baddrBits.W)
  val dst = UInt(baddrBits.W)
}

/** [[HistoryBuffer]] implements a history buffer, with BB heads labelled with timestamps.
  */
class HistoryBuffer(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO */
  val io = IO(new Bundle{
    val insert_req = Flipped(Valid(new HistoryBufferInsertReq))
    val search_req = Flipped(Decoupled(new HistoryBufferSearchReq))
    val search_resp = Valid(new HistoryBufferSearchResp)
  })

  /* Create the history buffer.
   * Setting the timestamps to the maximum value ensures they are not viewed
   * as a candidate for entangling before first assignment.
   */
  class HBBundle extends Bundle {
    val head = UInt(baddrBits.W)
    val time = UInt(timeBits.W)
  }
  val hist_buf = RegInit(VecInit(Seq.fill(histBufLen)((new HBBundle).Lit(_.time -> maxTime.U))))

  /* Insert a new significant block into the history buffer */
  when(io.insert_req.valid) {
    hist_buf(0).head := io.insert_req.bits.head
    hist_buf(0).time := io.insert_req.bits.time
    for (i <- 1 to histBufLen-1) {
      hist_buf(i).head := hist_buf(i-1).head
      hist_buf(i).time := hist_buf(i-1).time
    }
  }
  
  /* Define a bundle which will be used to store search data */
  class HBSearchBundle extends Bundle {
    val src = UInt(baddrBits.W)
    val dst = UInt(baddrBits.W)
    val target_time = UInt(timeBits.W)
    val valid = Bool()
    val found = Bool()
  }

  /* Define a register to store the state between iterations of the search,
   * and an iterator to indicate the current iteration
   */
  val search_status = RegInit((new HBSearchBundle).Lit(_.valid -> false.B))
  val search_iter = Reg(UInt(log2Up(histBufLen/histBufSearchFragLen+1).W))
  when(io.search_req.fire()) {
    search_iter := (histBufLen/histBufSearchFragLen).U
    search_status.src := DontCare
    search_status.dst := io.search_req.bits.dst
    search_status.target_time := io.search_req.bits.target_time
    search_status.valid := true.B
    search_status.found := false.B
  }

  /* Keep searching while the search register is valid */
  when(search_status.valid) {

    /* Invalidate the search register if a source is found or we reach the end of the buffer */
    when(search_status.found || search_iter === 0.U) {
      search_status.valid := false.B
    } 

    /* Otherwise we must not have found a result, so keep searching */
    .otherwise {
      
      /* Decrement the iterator */
      val search_iter1 = search_iter - 1.U
      search_iter := search_iter1

      /* Group the history buffer into groups of histBufSearchFragLen length, 
       * choose the search_iter'th group, and fold over that group.
       */ 
      search_status := VecInit(hist_buf
        .grouped(histBufSearchFragLen)
        .map(VecInit(_))
        .toSeq
        .reverse
      )(search_iter1).foldLeft(search_status)((prev_search_status, hb_entry) => {
        /* Define a wire which is the result of examining this history buffer entry */
        val next_search_status = WireDefault(prev_search_status)
        /* Only update this wire if the search is still valid and a source address has not been found */
        when(prev_search_status.valid && !prev_search_status.found) {
          /* If we see the destination address before finding a source address, then invalidate the search.
           * Else if we find a candidate source address, then save it.
           */
          when(hb_entry.head === search_status.dst) {
            next_search_status.valid := false.B
          } .elsewhen(hb_entry.time <= search_status.target_time) { 
            next_search_status.src := hb_entry.head
            next_search_status.found := true.B
          }
        }
        next_search_status
      })
    }
  }

  /* We are ready for another search request when the search register is invalid */
  io.search_req.ready := !search_status.valid

  /* Output the result of the search */
  io.search_resp.valid := search_status.valid && search_status.found
  io.search_resp.bits.src := search_status.src
  io.search_resp.bits.dst := search_status.dst

}



/** [[EntanglingTablePrefetchReq]] defines the interface for notifying the entangling table that a new basic block has started.
  */ 
class EntanglingTablePrefetchReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val baddr = UInt(baddrBits.W)
}

/** [[EntanglingTablePrefetchResp]] defines the interface for responding with BBs that should be prefetched.
  */ 
class EntanglingTablePrefetchResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val vidx = UInt(vidxBits.W)
  val size = UInt(lgMaxBBSize.W)
}

/** [[EntanglingTableUpdateReq]] defines the interface for updating a basic block size.
  */ 
class EntanglingTableUpdateReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val vidx = UInt(vidxBits.W)
  val size = UInt(lgMaxBBSize.W)
}

/** [[EntanglingTableEntangleReq]] defines the interface for requesting an entangling is made
  */ 
class EntanglingTableEntangleReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val src = UInt(baddrBits.W)
  val dst = UInt(baddrBits.W)
}

/** [[EntanglingTable]] implements an entangling table.
  * 
  * The I$ prefetcher sends three pieces of information to the entangling table:
  *    - A newly seen baddr, which may trigger a prefetch.
  *    - A just-completed basic block, so that its size can be updated in (or added to) the table.
  *    - A source and destination pair which should be entangled.
  * 
  * The entangling will respond only to notifications of newly seen baddrs with BBs to prefetch,
  * but only if that baddr happens to be present in the entangling table
  */
class EntanglingTable(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO.
   * The input IO is decoupled as it needs to be queued (as accessing the entangling table reduces what can be done at once).
   */
  val io = IO(new Bundle {
    val prefetch_req = Flipped(Decoupled(new EntanglingTablePrefetchReq))
    val update_req = Flipped(Decoupled(new EntanglingTableUpdateReq))
    val entangle_req = Flipped(Decoupled(new EntanglingTableEntangleReq))
    val prefetch_resp = Valid(new EntanglingTablePrefetchResp)
  })

  /* The prefetch response is invalid by default */
  io.prefetch_resp.valid := false.B
  io.prefetch_resp.bits := DontCare



  /* Define the state register */
  val s_ready :: s_prefetch_decode :: s_prefetch_output :: s_update_writeback :: s_entangle_encode :: s_entangle_writeback :: Nil = Enum(6)
  val state = RegInit(s_ready)



  /* Define queues for each of the request types */
  val prefetch_q = Queue(io.prefetch_req, eTablePrefetchQueueSize, flow=true)
  val update_q = Queue(io.update_req, eTableUpdateQueueSize)
  val entangle_q = Queue(io.entangle_req, eTableEntangleQueueSize)

  /* The queues are all by default not ready */
  prefetch_q.ready := false.B
  update_q.ready := false.B
  entangle_q.ready := false.B

  /* Define registers for holding each type of request */
  val prefetch_r = Reg(new EntanglingTablePrefetchReq)
  val update_r = Reg(new EntanglingTableUpdateReq)
  val entangle_r = Reg(new EntanglingTableEntangleReq)

  /* Registers for decoded entanglings */
  val prefetch_baddrs = Reg(Vec(maxEntanglings, UInt(baddrBits.W)))
  val prefetch_len = Reg(UInt(entanglingSizeBits.W))

  /* Define registers for holding onto whether an entangling table hit or missed */
  val read_hits_save = Reg(Vec(eTableNWays, Bool()))
  val read_hit_save = read_hits_save.reduce(_||_)



  /* Create an encoder and decoder (we only need one of each, they can be shared by the states) */
  val encoder = Module(new EntanglingEncoder)
  val decoder = Module(new EntanglingDecoder)

  /* Default the encoder inputs */
  encoder.io.req.valid := false.B
  encoder.io.req.bits := DontCare



  /* Define the tag and size SRAM. Each entry contains:
   *  - a validity bit in the LSB,
   *  - followed by the tag,
   *  - followed by the BB size.
   * If you try to use a bundle as the base type then the Verilog produced has a mask bit for _every_
   * bit of each way (rather than the same number of bits as ways).
   */
  val tag_size_array = DescribedSRAM(
    name = "tag_and_size_array",
    desc = "Entangling Prefetcher Tag and Size Array",
    size = eTableNSets,
    data = Vec(eTableNWays, Bits((lgMaxBBSize+vidxBits+entTagBits+1).W))
  )

  /* Define the entangling SRAM */
  val entangling_array = DescribedSRAM(
    name = "entangling_array",
    desc = "Entangling Prefetcher Entangling Array",
    size = eTableNSets,
    data = Vec(eTableNWays, Bits(entanglingBits.W))
  ) 



  /* Create wires that are always connected to the memory read port */
  val read_baddr = WireDefault(UInt(baddrBits.W), DontCare)
  val read_size_enable = WireDefault(false.B)
  val read_ents_enable = WireDefault(false.B)
  val read_enable = read_size_enable || read_ents_enable

  /* Save the read address for the next cycle */
  val read_baddr_prev = RegEnable(read_baddr, read_enable)

  /* Perform the read on the tags and sizes (we need to read the size to get the tag even if only entanglings are requested) */
  val read_raw_size_vidx_tag_valid = tag_size_array.read(read_baddr(entIdxBits-1, 0), read_enable).map(Split(_, vidxBits+entTagBits+1, entTagBits+1, 1))

  /* Perform the read on entanglings */
  val read_raw_ents = entangling_array.read(read_baddr(entIdxBits-1, 0), read_ents_enable)

  /* Check the tags and validity bits, asserting that we have a maximum of one hit */
  val read_hits = VecInit(read_raw_size_vidx_tag_valid.map(b => b._4(0) && b._3 === read_baddr_prev >> entIdxBits))
  val read_hit = read_hits.reduce(_||_)
  assert(!read_enable || PopCount(read_hits) <= 1.U)

  /* Get the read size */
  val read_size = Mux1H(read_hits, read_raw_size_vidx_tag_valid.map(_._1))
  val read_vidx = Mux1H(read_hits, read_raw_size_vidx_tag_valid.map(_._2))
  val read_ents = Mux1H(read_hits, read_raw_ents)

  /* Decode the entanglings */
  decoder.io.req.head := read_baddr_prev
  decoder.io.req.ents := read_ents
  val read_ents_baddrs = decoder.io.resp.baddrs
  val read_ents_len = decoder.io.resp.len



  /* Create wires that are always connected to the memory write port */
  val write_baddr = WireDefault(UInt(baddrBits.W), DontCare)
  val write_size = WireDefault(UInt(lgMaxBBSize.W), DontCare)
  val write_vidx = WireDefault(UInt(vidxBits.W), DontCare)
  val write_ents = WireDefault(UInt(entanglingBits.W), DontCare)
  val write_size_enable = WireDefault(false.B)
  val write_ents_enable = WireDefault(false.B)
  val write_enable = write_size_enable || write_ents_enable
  val write_mask = WireDefault(Vec(eTableNWays, Bool()), DontCare)
  val write_repl = WireDefault(false.B)

  /* Bundle the inputs */
  val write_raw_size_vidx_tag_valid = write_size ## write_vidx ## (write_baddr >> entIdxBits) ## true.B

  /* If write_repl is true, then we need to perform random replacement */
  val write_random_way = LFSR(8, write_repl && write_enable)(log2Up(eTableNWays)-1, 0)
  val write_random_mask = VecInit(UIntToOH(write_random_way).asBools)
  val write_true_mask = Mux(write_repl, write_random_mask, write_mask)

  /* If we are doing random replacement, then we expect to write both a size and entangling sequence (or neither).
   * Otherwise we expect the mask to have exactly one bit set.
   */
  assert(!write_repl || (write_size_enable === write_ents_enable))
  assert(write_repl || !write_enable || PopCount(write_mask) === 1.U)

  /* Perform the write on the tag and size (if write_size_enable is true) */
  when(write_size_enable) {
    tag_size_array.write(write_baddr(entIdxBits-1, 0), Seq.fill(eTableNWays)(write_raw_size_vidx_tag_valid), write_true_mask)
  }

  /* Perform the write on the entanglings (if write_ents_enable is true) */
  when(write_ents_enable) {
    entangling_array.write(write_baddr(entIdxBits-1, 0), Seq.fill(eTableNWays)(write_ents), write_true_mask)
  }



  /* Define the state transitions */
  switch(state) {

    /* Switch out of the ready state when there are requests to consume.
     * Prioritize prefetch requests.
     */
    is(s_ready) {
      /* When we can move into the prefetch state, read the size and entanglings array and move into the decode state */
      when(prefetch_q.valid) {
        readSizeAndEntanglings(prefetch_q.bits.baddr)
        prefetch_r := prefetch_q.deq() 
        state := s_prefetch_decode
      }
      
      /* When we can move into the update state, read the size array and move into the writeback state */
      .elsewhen(update_q.valid) { 
        readSize(update_q.bits.head)
        update_r := update_q.deq() 
        state := s_update_writeback
      } 
      
      /* When we can move into the entangle state, read memory and move to the encode state */
      .elsewhen(entangle_q.valid) { 
        readSizeAndEntanglings(entangle_q.bits.src)
        entangle_r := entangle_q.deq() 
        state := s_entangle_encode
      }
    }



    /* When we are in the update write state, we need to update the entangling table with a new size */
    is(s_update_writeback) {
      /* Update/insert the new size */
      when(!read_hit) {
        writeRandom(update_r.head, update_r.size, update_r.vidx)
      } .elsewhen(update_r.size > read_size || read_vidx =/= update_r.vidx) {
        writeAtWay(update_r.head, read_hits, Some((update_r.size, update_r.vidx)))
      }

      /* Move into the ready state */
      state := s_ready
    }



    /* When we are in the entangle encode state, encode the entanglings and write to memory */
    is(s_entangle_encode) {
      /* Check whether dst baddr is already entangled */
      val already_entangled = read_ents_baddrs.zipWithIndex.map{
        case (b, i) => b === entangle_r.dst && i.U < read_ents_len
      }.reduce(_||_)

      /* If the read was a miss or the dst is already entangled, then we don't want to entangle */
      when(!read_hit || already_entangled) {
        state := s_ready
      } .otherwise {
        /* Encode the entanglings */
        encoder.io.req.valid := true.B
        encoder.io.req.bits.head := entangle_r.src
        encoder.io.req.bits.baddrs := read_ents_baddrs.prepended(entangle_r.dst)
        encoder.io.req.bits.len := read_ents_len +& 1.U

        /* Move to the writeback state, but remember whether we hit or missed */
        read_hits_save := read_hits
        state := s_entangle_writeback
      }
    }

    /* When in the entangle writeback state, wait until encoding is done and then write */
    is(s_entangle_writeback) {
      when(encoder.io.resp.valid) {
        /* Perform the writeback (we know there was a hit) */
        writeAtWay(entangle_r.src, read_hits_save, None, Some(encoder.io.resp.bits.ents))

        /* Change to the ready state */
        state := s_ready
      }
    }



    /* When in the prefetch decode state, decode the entanglings and emit the first prefetch request */
    is(s_prefetch_decode) {
      /* We want to move back into the ready state if the cache missed */
      when(!read_hit) {
        state := s_ready
      } .otherwise {
        /* Output a prefetch request for this basic block.
         * Don't include the head itself: if that address missed then it has already been requested.
         */
        io.prefetch_resp.valid := read_size > 1.U
        io.prefetch_resp.bits.head := prefetch_r.baddr + 1.U
        io.prefetch_resp.bits.vidx := read_vidx
        io.prefetch_resp.bits.size := read_size - 1.U

        /* If there are no dst entangled addresses then move to the ready state */
        when(read_ents_len === 0.U) {
          state := s_ready
        } .otherwise {
          /* Save the decoder output */
          prefetch_baddrs := read_ents_baddrs
          prefetch_len := read_ents_len - 1.U

          /* Initiate the first read */
          readSize(read_ents_baddrs(read_ents_len-1.U))

          /* Swap to the output state */
          state := s_prefetch_output
        }
      }
    }

    /* When in the prefetch output state, keep outputting entanglings */
    is(s_prefetch_output) {
      /* Respond with the basic block on a cache hit */
      when(read_hit) {
        io.prefetch_resp.valid := true.B
        io.prefetch_resp.bits.head := prefetch_baddrs(prefetch_len)
        io.prefetch_resp.bits.vidx := read_vidx
        io.prefetch_resp.bits.size := read_size
      }

      /* If there are no dst entangled addresses then move to the ready state */
      when(prefetch_len === 0.U) {
        state := s_ready
      } .otherwise {
        /* Decrement the length and trigger the next read */
        prefetch_len := prefetch_len - 1.U
        readSize(prefetch_baddrs(prefetch_len-1.U))
      }
    }

  }



  /** Trigger a read of the size of an address in the entangling table.
    *
    * @param baddr The address to search for.
    */
  def readSize(baddr: UInt): Unit = {
    read_baddr := baddr
    read_size_enable := true.B
  }

  /** Trigger a read of the size of an address and its entanglings in the entangling table.
    *
    * @param baddr The address to search for.
    */
  def readSizeAndEntanglings(baddr: UInt): Unit = {
    read_baddr := baddr
    read_size_enable := true.B
    read_ents_enable := true.B
  }

  /** Trigger a write of a size and/or entangling to the entangling table at a known way.
    *
    * @param baddr The address to write to.
    * @param mask The one-hot mask.
    * @param size_vidx Optionally, the size and vidx to write.
    * @param ents Optionally, the entangling sequence to write.
    */
  def writeAtWay(baddr: UInt, mask: Vec[Bool], size_vidx: Option[(UInt, UInt)], ents: Option[Bits] = None): Unit = {
    write_baddr := baddr
    if(size_vidx.isDefined) {
      write_size := size_vidx.get._1
      write_vidx := size_vidx.get._2
      write_size_enable := true.B
    } 
    if(ents.isDefined) {
      write_ents := ents.get
      write_ents_enable := true.B
    }
    write_mask := mask
  }

  /** Trigger the insertion of a size and entangling sequence to the entangling table.
    *
    * @param baddr The address to write to.
    * @param size The size to write.
    * @param ents The entangling sequence.
    */
  def writeRandom(baddr: UInt, size: UInt, vidx: UInt, ents: Bits = 0.U): Unit = {
    write_baddr := baddr
    write_size := size
    write_vidx := vidx
    write_ents := ents
    write_size_enable := true.B
    write_ents_enable := true.B
    write_repl := true.B
  }

}



/** [[PrefetchQueue]] takes in blocks to be prefetched and exposes them as single addresses.
  */
class PrefetchQueue(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO */
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new EntanglingTablePrefetchResp))
    val resp = Decoupled(new EntanglingIPrefetcherPrefetchResp)
  })

  /* Queue the input. The queue leads straight into a register, 
   * so it makes sense to enable flow-through. 
   */
  val req_q = Queue(io.req, prefetchQueueSize, flow=true)

  /* The current BB being distributed */
  val current_req = RegInit((new EntanglingTablePrefetchResp).Lit(_.size -> 0.U))

  /* Update the current BB being distributed */
  when(req_q.valid && ((io.resp.fire() && current_req.size === 1.U) || current_req.size === 0.U)) {
    current_req := req_q.deq()
  } .elsewhen (io.resp.fire() && current_req.size =/= 0.U) {
    current_req.head := current_req.head + 1.U
    current_req.size := current_req.size - 1.U
  }

  /* Output the next address */
  io.resp.valid := current_req.size =/= 0.U
  io.resp.bits.paddr := current_req.head << blockOffBits
  io.resp.bits.index := current_req.vidx ## (current_req.head)(((pgIdxBits-blockOffBits) min idxBits)-1,0)
  req_q.ready := io.resp.ready

}



/** [[EntanglingIPrefetcherFetchReq]] defines the interface for notifying the prefetcher of a fetch.
  */ 
class EntanglingIPrefetcherFetchReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val paddr = UInt(paddrBits.W)
  val index = UInt(idxBits.W)
}

/** [[EntanglingIPrefetcherMissReq]] defines the interface for notifying the prefetcher of a cache miss.
 */
class EntanglingIPrefetcherMissReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val paddr = UInt(paddrBits.W)
}

/** [[EntanglingIPrefetcherPrefetchResp]] defines the interface for responding with prefetches.
 */
class EntanglingIPrefetcherPrefetchResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val paddr = UInt(paddrBits.W)
  val index = UInt(idxBits.W)
}

/** [[EntanglingIPrefetcher]] implements an entangling instruction prefetcher
  * 
  * The I$ sends two pieces of information to the prefetcher:
  *    - The physical addresses that are requested.
  *      These are used to trigger prefetches, as well as record basic blocks.
  *    - The address and fetch time for a demand miss.
  *      This allows the prefetcher to create new entanglings.
  * 
  * The prefetcher will respond with basic blocks that it thinks should be prefetched.
  */
class EntanglingIPrefetcher(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* The io for the prefetcher */
  val io = IO(new Bundle {
    val fetch_req = Flipped(Valid(new EntanglingIPrefetcherFetchReq))
    val miss_req = Flipped(Valid(new EntanglingIPrefetcherMissReq))
    val prefetch_resp = Decoupled(new EntanglingIPrefetcherPrefetchResp)
    val time = Input(UInt(timeBits.W))
  })



  /* Create the BB counter */
  val bb_counter = Module(new BBCounter)

  /* Connect its input IO */
  val fetch_baddr = io.fetch_req.bits.paddr >> blockOffBits
  val fetch_vidx = if (vidxBits == 0) DontCare else io.fetch_req.bits.index >> (pgUntagBits-blockOffBits)
  bb_counter.io.req.valid := io.fetch_req.valid
  bb_counter.io.req.bits.baddr := fetch_baddr
  bb_counter.io.req.bits.vidx := fetch_vidx
  bb_counter.io.req.bits.time := io.time



  /* Create the history buffer */
  val history_buffer = Module(new HistoryBuffer)

  /* Link up the insertion IO */
  history_buffer.io.insert_req.valid := bb_counter.io.resp.done
  history_buffer.io.insert_req.bits.head := bb_counter.io.resp.head
  history_buffer.io.insert_req.bits.time := bb_counter.io.resp.time

  /* Link up the search IO. We don't need to search if
   *  - the miss is invalid, or
   *  - the miss address isn't for this BB's head.
   * Assert that the history buffer is ready for a search. If it isn't then something strange has happened:
   * a search request only comes in on a refill finishing, and a refill takes at least eight cycles.
   */
  val miss_baddr = io.miss_req.bits.paddr >> blockOffBits
  assert(!history_buffer.io.search_req.valid || history_buffer.io.search_req.ready)
  history_buffer.io.search_req.valid := io.miss_req.valid && miss_baddr === bb_counter.io.resp.head
  history_buffer.io.search_req.bits.dst := miss_baddr
  history_buffer.io.search_req.bits.target_time := bb_counter.io.resp.time - (io.time - bb_counter.io.resp.time)



  /* Create the entangling table */
  val entangling_table = Module(new EntanglingTable)

  /* We don't want to keep asking the entangling table to look up the same baddr.
   * Only look up if we're extending the current basic block.
   */
  entangling_table.io.prefetch_req.valid := io.fetch_req.valid && (fetch_baddr < bb_counter.io.resp.head || fetch_baddr >= bb_counter.io.resp.head + bb_counter.io.resp.size)
  entangling_table.io.prefetch_req.bits.baddr := fetch_baddr

  /* Link up the new BB IO */
  entangling_table.io.update_req.valid := bb_counter.io.resp.done
  entangling_table.io.update_req.bits.head := bb_counter.io.resp.head
  entangling_table.io.update_req.bits.vidx := bb_counter.io.resp.vidx
  entangling_table.io.update_req.bits.size := bb_counter.io.resp.size

  /* Link up the entangling creation IO.
   * We don't want to entangle an address with itself. 
   * If the the history buffer search produced a source address equal to the destination,
   * then we just won't make an entangling.
   */
  entangling_table.io.entangle_req.valid := history_buffer.io.search_resp.valid && entangling_table.io.entangle_req.bits.src =/= entangling_table.io.entangle_req.bits.dst
  entangling_table.io.entangle_req.bits.src := history_buffer.io.search_resp.bits.src
  entangling_table.io.entangle_req.bits.dst := history_buffer.io.search_resp.bits.dst



  /* Create the prefetch queue */
  val prefetch_queue = Module(new PrefetchQueue)

  /* Connect the prefetch queue IO to the entangling table and the final prefetch output */
  prefetch_queue.io.req.valid := entangling_table.io.prefetch_resp.valid
  prefetch_queue.io.req.bits := entangling_table.io.prefetch_resp.bits
  io.prefetch_resp <> prefetch_queue.io.resp

}



/** [[EntanglingTest]] instruments tests for block address compression. 
  *
  * @param id The ID of the test (for debugging purposes).
  * @param head The head baddr that we are encoding/decoding against.
  * @param baddrs A sequence of baddrs to encode.
  * @param exp_drops The expected number of baddrs that should be dropped during the encoding process.
  * 
  * During the test, the input baddrs are encoded and then immediately encoded. We expect that
  *   - all baddrs we receive from decoding were baddrs that we encoded,
  *   - the expected number of baddrs were dropped during encoding,
  *   - and the first baddr in the input sequence is present in the output sequence.
  */
class EntanglingTest(id: Int, head: Int, baddrs: Seq[Int], exp_drops: Int)(implicit val p: Parameters) 
    extends UnitTest 
    with HasEntanglingIPrefetcherParameters 
{
  /* The tests must be within a UnitTestModule */
  class Impl extends UnitTestModule {
    /* Create an encoder and decoder */
    val encoder = Module(new EntanglingEncoder)
    val decoder = Module(new EntanglingDecoder)

    /* Create a ROM for baddr sequence */
    val test_baddrs_in = VecInit(baddrs.appendedAll(Seq.fill(maxEntanglings+1-baddrs.length)(0)).map(_.U(baddrBits.W)))

    /* Perform the encoding */
    encoder.io.req.bits.len := baddrs.length.U
    encoder.io.req.bits.baddrs := test_baddrs_in
    encoder.io.req.bits.head := head.U(baddrBits.W)
    encoder.io.req.valid := io.start

    /* Perform the decoding */
    decoder.io.req.ents := encoder.io.resp.bits.ents
    decoder.io.req.head := encoder.io.resp.bits.head

    /* Save the decoding output */
    val test_len_out = decoder.io.resp.len 
    val test_baddrs_out = decoder.io.resp.baddrs 
    val test_valid_out = encoder.io.resp.valid

    /* A register for when we have finished the test */
    val test_finished = RegInit(false.B)

    /* Perform assertions */
    when(test_valid_out) {
      /* Check that we have the right amount of drops */
      assert(test_len_out === (baddrs.length - exp_drops).U,
        s"[EntanglerTest $id]: baddrs.length: ${baddrs.length}, test_len_out: %d, exp_drops: $exp_drops\n", 
        test_len_out
      )

      /* Check that each of the output baddrs are present in the input sequence */
      for(i <- 0 until maxEntanglings) {
        when(i.U < test_len_out) {
          assert(test_baddrs_in.contains(test_baddrs_out(i)),
            s"[EntanglerTest $id] baddr %x present in the output, but not the input!", 
            test_baddrs_out(i)
          )
        }
      }

      /* Check that the first input address is present in the output addresses */
      if(baddrs.length > 0) {
        assert(test_baddrs_out.contains(test_baddrs_in(0)),
          s"[EntanglerTest $id] random replacement dropped the first baddr!", 
        )
      }

      /* The test has now succeeded */
      test_finished := true.B
    }

    /* Output the success register */
    io.finished := test_finished
  }

  /* Instantiate the test module */
  val dut = Module(new Impl)

  /* Connect the UnitTest IO to the UnitTestModule IO */
  dut.io.start := io.start 
  io.finished := dut.io.finished
}



/** [[BBCounterTest]] instruments tests for basic block accumulation. 
  *
  * @param id The ID of the test (for debugging purposes).
  * @param in The sequence of requests to make to the BB counter, as well as a validity flag.
  * @param out The sequence of responses to expect from the BB counter. The corresponding response for 
  * a request is checked the cycle after that request is made. 
  */
class BBCounterTest(id: Int, in: Seq[(BBCounterReq, Bool)], out: Seq[BBCounterResp])(implicit val p: Parameters) 
    extends UnitTest 
    with HasEntanglingIPrefetcherParameters 
{
  /* The tests must be within a UnitTestModule */
  class Impl extends UnitTestModule {

    /* Create the BB counter */
    val bb_counter = Module(new BBCounter)

    /* Create ROM arrays for the test sequences */
    assert(in.length == out.length)
    val in_bits_rom = RegInit(VecInit(in.map(_._1)))
    val in_valid_rom = RegInit(VecInit(in.map(_._2)))
    val out_rom = RegInit(VecInit(out))

    /* Block until we have started */
    val started = RegEnable(io.start, false.B, io.start)
    
    /* Create an iterator for passing through the inputs and outputs */
    val i = RegInit(0.U(log2Up(in.length+2).W))
    when(started && i < (in.length+1).U) { i := i + 1.U }

    /* Pass-in the inputs */
    bb_counter.io.req.valid := started && i < in.length.U && in_valid_rom(i)
    bb_counter.io.req.bits := in_bits_rom(i)

    /* Check the outputs */
    when(i > 0.U && i <= out.length.U) {
      val j = i-1.U
      assert(bb_counter.io.resp === out_rom(j), 
        s"[BBCounterTest $id] i=%d expected {%x, %d, %d, [%d]} after request {%x, %d, [%d]}, but got {%x, %d, %d, [%d]} ",
        i, out_rom(j).head, out_rom(j).time, out_rom(j).size, out_rom(j).done,
        in_bits_rom(j).baddr, in_bits_rom(j).time, in_valid_rom(j),
        bb_counter.io.resp.head, bb_counter.io.resp.time, bb_counter.io.resp.size, bb_counter.io.resp.done,
      )
    }

    /* We are done if we get through all of the inputs without failing */
    io.finished := i === (in.length+1).U
  }

  /* Instantiate the test module */
  val dut = Module(new Impl)

  /* Connect the UnitTest IO to the UnitTestModule IO */
  dut.io.start := io.start 
  io.finished := dut.io.finished
}



/** [[HistoryBufferTest]] instruments tests for inserting and searching the history buffer. 
  * 
  * @param id The ID of the test (for debugging purposes).
  * @param in A sequence of insert requests to make to the history buffer.
  * @param delay A delay (in cycles) before making the search requests.
  * @param query A search request to make after delay cycles.
  * @param out A response to expect from the search request, or no response.
  */
class HistoryBufferTest(id: Int, in: Seq[HistoryBufferInsertReq], delay: Int, query: HistoryBufferSearchReq, out: Option[HistoryBufferSearchResp])(implicit val p: Parameters) 
    extends UnitTest 
    with HasEntanglingIPrefetcherParameters 
{
  /* The tests must be within a UnitTestModule */
  class Impl extends UnitTestModule {
    
    /* Create the history buffer */
    val history_buffer = Module(new HistoryBuffer)

    /* Create ROM array for the insertion sequence */
    val in_rom = VecInit(in)

    /* Block until we have started */
    val started = RegEnable(io.start, false.B, io.start)

    /* Create registers to iterate over the inputs/outputs */
    def maxIterations = in.length.max(delay+maxHistBufSearchLatency+1)
    val i = RegInit(0.U(log2Up(maxIterations+1).W))

    /* Iterate i */
    when(started && i =/= maxIterations.U) { i := i + 1.U }

    /* Wire up the insert requests */
    history_buffer.io.insert_req.valid := started && i < in.length.U
    history_buffer.io.insert_req.bits := in_rom(i)

    /* Wire up the search request */
    history_buffer.io.search_req.valid := started && i === delay.U
    history_buffer.io.search_req.bits := query

    /* Remember if an output has been seen */ 
    val seen_output = RegInit(false.B)
    when(!seen_output) { seen_output := history_buffer.io.search_resp.valid }

    /* Check the search results */
    if(out.isDefined) {
      assert(!seen_output || !history_buffer.io.search_resp.valid,
        s"[HistoryBufferTest $id] received second output {src:%x, dst:%x}",
        history_buffer.io.search_resp.bits.src, history_buffer.io.search_resp.bits.dst
      )
      assert(!history_buffer.io.search_resp.valid || history_buffer.io.search_resp.bits === out.get,
        s"[HistoryBufferTest $id] expected result {src:%x, dst:%x}, but got {src:%x, dst:%x}",
        out.get.src, out.get.dst, history_buffer.io.search_resp.bits.src, history_buffer.io.search_resp.bits.dst
      )
      assert(seen_output || i =/= maxIterations.U, 
        s"[HistoryBufferTest $id] no output received, but expected {src:%x, dst:%x}",
        out.get.src, out.get.dst
      )
    } else {
      assert(!history_buffer.io.search_resp.valid,
        s"[HistoryBufferTest $id] expected no output, but got {src: %x, dst: %x}",
        history_buffer.io.search_resp.bits.src, history_buffer.io.search_resp.bits.dst
      )
    }

    /* Set the finished flag */
    io.finished := i === maxIterations.U
  }

  /* Instantiate the test module */
  val dut = Module(new Impl)

  /* Connect the UnitTest IO to the UnitTestModule IO */
  dut.io.start := io.start 
  io.finished := dut.io.finished
}



/** [[EntanglingTableTest]] instruments tests for the entangling table.
  *
  * @param id The ID of the test (for debugging purposes).
  */
class EntanglingTableTest(
  id: Int, 
  updateReq: Seq[EntanglingTableUpdateReq],
  entangleReq: Seq[EntanglingTableEntangleReq],
  prefetchReq: Seq[EntanglingTablePrefetchReq],
  prefetchResp: Seq[EntanglingTablePrefetchResp],
)(implicit val p: Parameters) extends UnitTest with HasEntanglingIPrefetcherParameters {
  /* The tests must be within a UnitTestModule */
  class Impl extends UnitTestModule {
    
    /* Create the entangling table */
    val entangling_table = Module(new EntanglingTable)

    /* Create ROMS for the test inputs/outputs (make sure none of them are empty) */
    val update_rom = VecInit(updateReq.appended((new EntanglingTableUpdateReq).Lit()))
    val entangle_rom = VecInit(entangleReq.appended((new EntanglingTableEntangleReq).Lit()))
    val prefetch_req_rom = VecInit(prefetchReq.appended((new EntanglingTablePrefetchReq).Lit()))
    val prefetch_resp_rom = VecInit(prefetchResp.appended((new EntanglingTablePrefetchResp).Lit()))

    /* Create a counter to time sending requests.
     * Assume each request can take a maximum of 8 clock cycles to complete.
     * This is a very high upper bound to be on the safe side.
     */
    val req_full_iter = RegInit(0.U(16.W))
    val req_iter = req_full_iter >> 3
    val req_fire = req_full_iter(2,0) === 0.U

    /* Indices for when to start each type of request */
    val start_entangle_req = updateReq.length
    val start_prefetch_req = start_entangle_req + entangleReq.length
    val end_prefetch_req = start_prefetch_req + prefetchReq.length
    val end_iteration = end_prefetch_req + 1

    /* Block until we have started */
    val started = RegEnable(io.start, false.B, io.start)

    /* Increment the iterator */
    when(started && req_iter < end_iteration.U) { req_full_iter := req_full_iter + 1.U }

    /* Make the update requests */
    entangling_table.io.update_req.valid := started && req_fire && req_iter < start_entangle_req.U
    entangling_table.io.update_req.bits := update_rom(req_iter)

    /* Make the entangle requests */
    entangling_table.io.entangle_req.valid := req_fire && req_iter >= start_entangle_req.U && req_iter < start_prefetch_req.U
    entangling_table.io.entangle_req.bits := entangle_rom(req_iter -& start_entangle_req.U)

    /* Make the prefetch requests */
    entangling_table.io.prefetch_req.valid := req_fire && req_iter >= start_prefetch_req.U && req_iter < end_prefetch_req.U
    entangling_table.io.prefetch_req.bits := prefetch_req_rom(req_iter -& start_prefetch_req.U)

    /* Check the results of prefetching */
    val resp_iter = RegInit(0.U(log2Up(prefetchResp.length+1).W))
    when(entangling_table.io.prefetch_resp.valid) {
      assert(resp_iter < prefetchResp.length.U,
        s"[EntanglingTableTest $id] extra prefetch request of {%x, %d}\n",
        entangling_table.io.prefetch_resp.bits.head, entangling_table.io.prefetch_resp.bits.size
      )
      assert(entangling_table.io.prefetch_resp.bits === prefetch_resp_rom(resp_iter),
        s"[EntanglingTableTest $id] output index %d mismatch: expected {%x, %d}, but got {%x, %d}\n",
        resp_iter, prefetch_resp_rom(resp_iter).head, prefetch_resp_rom(resp_iter).size,
        entangling_table.io.prefetch_resp.bits.head, entangling_table.io.prefetch_resp.bits.size 
      )
      resp_iter := resp_iter + 1.U
    }

    /* Check that we got all of the outputs */
    when(req_fire && req_iter === end_iteration.U) {
      assert(resp_iter === prefetchResp.length.U,
        s"[EntanglingTableTest $id] expected ${prefetchResp.length} responses, but got %d\n",
        resp_iter
      )
    } 

    /* Set the finished flag */
    io.finished := req_iter === end_iteration.U
  }

  /* Instantiate the test module */
  val dut = Module(new Impl)

  /* Connect the UnitTest IO to the UnitTestModule IO */
  dut.io.start := io.start 
  io.finished := dut.io.finished
}