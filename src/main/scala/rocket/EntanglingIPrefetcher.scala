package freechips.rocketchip.rocket

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.conversions._
import chisel3.util.{Cat, Decoupled, Enum, Mux1H, OHToUInt, PopCount, Queue, RegEnable, ShiftRegister, UIntToOH, Valid, isPow2, log2Up, switch, is}
import chisel3.util.random.LFSR
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.unittest._
import freechips.rocketchip.util.{DescribedSRAM, Split}
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
  histBufLen: Int = 8,
  /* The number of elements of the history buffer to search in one combinatorial path */
  histBufSearchFragLen: Int = 4,
  /* The ways and sets of the entangling table */
  nWays: Int = 4,
  nSets: Int = 1024,
  /* The queue sizes for entangling table IO */
  entPrefQueueSize: Int = 8,
  entUpdateQueueSize: Int = 4,
  entEntangleQueueSize: Int = 2,
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
  def entNWays = entanglingParams.nWays
  def entNSets = entanglingParams.nSets
  def entPrefQueueSize = entanglingParams.entPrefQueueSize
  def entUpdateQueueSize = entanglingParams.entUpdateQueueSize
  def entEntangleQueueSize = entanglingParams.entEntangleQueueSize

  /* The block address size */
  def baddrBits = paddrBits - blockOffBits

  /* Configuration for the entangling table */
  require(isPow2(nWays), "entanglingNSets must be a power of 2")
  def entIdxBits = log2Up(nSets)
  def entTagBits = baddrBits - entIdxBits

  /* The number of bits required to store a BB size */
  def lgMaxBBSize = log2Up(maxBBSize + 1).toInt

  /* the history buffer search latency */
  def histBufSearchLatency = math.ceil(histBufLen/histBufSearchFragLen).toInt + 1

  /* The maximum possible baddr and time */
  def maxTime = (math.pow(2, timeBits)-1).toInt
  def maxBaddr = (math.pow(2, baddrBits)-1).toInt
}



/** [[EntanglerEncodeReq]] A request to encode, or response to decode for entangling operations.
  */ 
class EntanglerEncodeReq(nBaddrs: Int)(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val baddrs = Vec(nBaddrs, UInt(baddrBits.W))
  val len = UInt(3.W)
}

/** [[EntanglerEncodeResp]] A response to encode, or a request to decode for entangling operations.
  */ 
class EntanglerEncodeResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val ents = Bits(63.W)
}

/** [[EntanglingEncoder]] compresses baddrs into a single 63-bit value.
  * The process can take a varying number of cycles depending on how many addresses need to be dropped.
  * For this reason, the IO is decoupled. 
  *
  * @param keepFirstBaddr Whether the first baddr in the input sequence should be dropped by random replacement.
  * This is useful when a newly-added baddr is always the first in the input sequence. 
  */
class EntanglingEncoder(keepFirstBaddr: Boolean = true)(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO. The encoding process can take multiple cycles, so use decoupled IO. */
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new EntanglerEncodeReq(7)))
    val resp = Valid(new EntanglerEncodeResp)
  })

  /* Create registers for each of the inputs */
  val req = Reg(new EntanglerEncodeReq(7))

  /* Create a register for whether we are busy or not */
  val busy = RegInit(false.B)

  /* We want to inform the user whether we are busy or not */
  io.req.ready := !busy

  /* Set the default output ports */
  io.resp.valid := false.B
  io.resp.bits.head := req.head
  io.resp.bits.ents := DontCare

  /* If we are ready and there is available input, then consume it */
  when(!busy && io.req.valid) {
    busy := true.B
    req := io.req.bits
  }

  /* If we are in the busy state, then continue with the ongoing encoding */
  when(busy) {
    /* The number of bits each baddr will be compressed to.
     * Division by 0 is fine here, as entryBits won't actually be used in that case.
     */
    val entryBits = 60.U / req.len

    /* Consider whether we can perform the compression. We must have
     *  - six or fewer addresses to entangle, and
     *  - each address must share the same MSBs as the head.
     */
    val baddrs_okay = req.len <= 6.U && req.baddrs.zipWithIndex.map{
      case (baddr, i) => (baddr >> entryBits) === (req.head >> entryBits) || i.U >= req.len
    }.reduce(_&&_)

    /* We can produce an output entangling sequence when baddrs_okay is flagged.
     * ents is initialized to 0, which is the correct output for the case where req.len is 0.
     */
    when(baddrs_okay) {
      busy := false.B
      io.resp.valid := true.B
      for(i <- 1 to 6) {
        when(i.U === req.len) {
          io.resp.bits.ents := req.len ## req.baddrs.take(i).map(_.pad(60/i)(60/i-1,0)).reduce(_##_)
        }
      }
    } 
    
    /* Otherwise (baddrs_okay is false) we need to randomly pop one of the addresses */
    .otherwise {
      /* Generate a random number for which index is to be evicted */
      assert(req.len > 1.U)
      val rnd = LFSR(8) % (if (keepFirstBaddr) req.len - 1.U else req.len)

      /* Move the last register to the position of the evictee */
      req.baddrs(if (keepFirstBaddr) rnd+1.U else rnd) := req.baddrs(req.len-1.U)
      
      /* Decrement the length register */
      req.len := req.len - 1.U
    }
  }  
}

/** [[EntanglingDecoder]] Decompresses compressed baddrs into separate registers and a length indicator.
  * The process is purely combinatorial logic (bit splicing), so the IO is not even Valid.
  */
class EntanglingDecoder(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO. We don't need decoupled IO as decoding only takes one cycle. */
  val io = IO(new Bundle {
    val req = Input(new EntanglerEncodeResp)
    val resp = Output(new EntanglerEncodeReq(6))
  })

  /* Get the mode of the entangling */
  val mode = io.req.ents(62,60)

  /* Set the the output. We will overwrite baddrs. */
  io.resp.head := io.req.head
  io.resp.baddrs := DontCare
  io.resp.len := mode

  /* Iterate over each of the modes, and hardware branch over each iteration equalling the mode.
   * Note that this also works when the mode is 0, and that we don't care about the output vector in that case.
   */
  for (i <- 1 to 6) {
    when(i.U === mode) {

      /* This is the number of bits for each entangling entry in this mode */
      val entryBits = 60 / i

      /* Iterate over the number of addresses we need to extract */
      for (j <- 0 until i) {
        /* Detect whether the entire address fits in this entry, which makes setting the output easier */
        if (entryBits >= baddrBits) {
          io.resp.baddrs(i-j-1) := io.req.ents(baddrBits+j*entryBits-1,j*entryBits)
        /* Else we need to reconstruct the address based on the head */
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
  val time = UInt(timeBits.W)
}

/** [[BBCounterSearchReq]] defines the interface for when a BB is completed.
  */
class BBCounterResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
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
  val bb_head = RegInit(0.U(baddrBits.W))
  val bb_size = RegInit(0.U(lgMaxBBSize.W))
  val bb_time = RegInit(0.U(timeBits.W))

  /* Calculate the change in io.req.bits.baddr compared to the current head.
   * We need to be quite careful regarding overflow while performing these calculations.
   * bb_cont indicates that we are within the same BB (true if baddr is not valid).
   * bb_grow indicates that we have just extended the BB size (false if baddr is not valid).
   */
  val req_bdiff = io.req.bits.baddr - bb_head
  val bb_cont = !io.req.valid || (io.req.bits.baddr >= bb_head && req_bdiff <= bb_size +& maxBBGapSize.U && req_bdiff < maxBBSize.U)
  val bb_grow = io.req.valid && bb_cont && req_bdiff >= bb_size

  /* Update the registers */
  when(!bb_cont || bb_size === 0.U) { 
    bb_head := io.req.bits.baddr 
    bb_time := io.req.bits.time 
    bb_size := 1.U
  } .elsewhen(bb_grow) { 
    bb_size := req_bdiff + 1.U
  }

  /* Potentially output a completed significant BB. This is when a basic block reaches a size of sigBBSize.
   * This can stop tiny basic blocks from filling the entangling table.
   */
  io.resp.done := !bb_cont && bb_size >= sigBBSize.U
  io.resp.head := bb_head
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
  val target_time = UInt(timeBits.W)
}

/** [[HistoryBufferSearchResp]] defines the interface for responding to a HB search request.
  * If the search is unsuccessful, then no response is made. 
  */
class HistoryBufferSearchResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
}

/** [[HistoryBuffer]] implements a history buffer, with BB heads labelled with timestamps.
  */
class HistoryBuffer(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO */
  val io = IO(new Bundle{
    val insert_req = Flipped(Valid(new HistoryBufferInsertReq))
    val search_req = Flipped(Valid(new HistoryBufferSearchReq))
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

  /* Here, we want to search through the history buffer for the first entry with a timestamp
   * earlier than the target timestamp.
   * One option is to create a big combinatorial circuit across the entire buffer in order to 
   * find a match in a single clock cycle. However, this would be a very long combinatorial circuit.
   * Another option is to examine each entry in the history buffer in a separate cycle. Latency of the lookup
   * isn't a problem, but this would require a lot of extra registers.
   * To balance this we can mix the above: split the history buffer into groups of histBufSearchFragLen size,
   * and perform the search as a pipeline, taking one cycle to examine each group.
   * In order to do this, we will define a bundle which will store the state for each
   * stage of the search pipeline.
   */
  class HBSearchResultBundle extends Bundle {
    val head = UInt(baddrBits.W)
    val valid = Bool()
  }
  class HBSearchBundle extends Bundle {
    val target_time = UInt(timeBits.W)
    val result = new HBSearchResultBundle
    val valid = Bool()
  }

  /* Define the initial state of the search pipeline */
  val hb_search_init = RegInit((new HBSearchBundle).Lit(_.valid -> false.B))
  when(io.search_req.valid) {
    hb_search_init.target_time := io.search_req.bits.target_time
    hb_search_init.result.head := DontCare
    hb_search_init.result.valid := false.B
  }
  hb_search_init.valid := io.search_req.valid
 
  /* Now group up the history buffer and fold over the stages of the pipeline */
  val hb_search_result = hist_buf.grouped(histBufSearchFragLen).foldLeft(hb_search_init)((prev, h) => {
    val next = RegInit((new HBSearchBundle).Lit(_.valid -> false.B))
    when(prev.valid) {
      next.target_time := prev.target_time
      next.result := h.foldLeft(prev.result)((p, x) => {
        val w = Wire(new HBSearchResultBundle)
        when (!p.valid && x.time <= prev.target_time) { 
          w.head := x.head
          w.valid := true.B
        } .otherwise { w := p }
        w
      })
    }
    next.valid := prev.valid
    next
  })

  /* Output the result of the search */
  io.search_resp.valid := hb_search_result.valid && hb_search_result.result.valid
  io.search_resp.bits.head := hb_search_result.result.head

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
  val size = UInt(lgMaxBBSize.W)
}

/** [[EntanglingTableUpdateReq]] defines the interface for updating a basic block size.
  */ 
class EntanglingTableUpdateReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
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
    val pref_req = Flipped(Decoupled(new EntanglingTablePrefetchReq))
    val update_req = Flipped(Decoupled(new EntanglingTableUpdateReq))
    val entangle_req = Flipped(Decoupled(new EntanglingTableEntangleReq))
    val pref_resp = Valid(new EntanglingTablePrefetchResp)
  })



  /* Define the state register */
  val s_ready :: s_pref_decode :: s_pref_output :: s_update_writeback :: s_entangle_encode :: s_entangle_writeback :: Nil = Enum(6)
  val state = RegInit(s_ready)



  /* Define queues for each of the request types */
  val pref_q = Queue(io.pref_req, entPrefQueueSize)
  val update_q = Queue(io.update_req, entUpdateQueueSize)
  val entangle_q = Queue(io.entangle_req, entEntangleQueueSize)

  /* The queues are all by default not ready */
  pref_q.ready := false.B
  update_q.ready := false.B
  entangle_q.ready := false.B

  /* Define registers for holding each type of request */
  val pref_r = Reg(new EntanglingTablePrefetchReq)
  val update_r = Reg(new EntanglingTableUpdateReq)
  val entangle_r = Reg(new EntanglingTableEntangleReq)

  /* Registers for decoded entanglings */
  val pref_baddrs = Reg(Vec(6, UInt(baddrBits.W)))
  val pref_len = Reg(UInt(3.W))

  /* Define registers for holding onto whether an entangling table hit or missed */
  val read_hits_save = Reg(Vec(entNWays, Bool()))
  val read_hit_save = read_hits_save.reduce(_||_)



  /* The prefetch response is invalid by default */
  io.pref_resp.valid := false.B
  io.pref_resp.bits := DontCare



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
    size = entNSets,
    data = Vec(entNWays, Bits((lgMaxBBSize+entTagBits+1).W))
  )

  /* Define the entangling SRAM */
  val entangling_array = DescribedSRAM(
    name = "entangling_array",
    desc = "Entangling Prefetcher Entangling Array",
    size = entNSets,
    data = Vec(entNWays, Bits(63.W))
  ) 



  /* Create wires that are always connected to the memory read port */
  val read_baddr = WireDefault(UInt(baddrBits.W), DontCare)
  val read_size_enable = WireDefault(false.B)
  val read_ents_enable = WireDefault(false.B)
  val read_enable = read_size_enable || read_ents_enable

  /* Perform the read on the tags and sizes (we need to read the size to get the tag even if only entanglings are requested) */
  val read_raw_size_tag_valid = tag_size_array.read(read_baddr(entIdxBits-1, 0), read_enable).map(Split(_, entTagBits+1, 1))

  /* Perform the read on entanglings */
  val read_raw_ents = entangling_array.read(read_baddr(entIdxBits-1, 0), read_ents_enable)

  /* Check the tags and validity bits, asserting that we have a maximum of one hit */
  val read_hits = VecInit(read_raw_size_tag_valid.map(b => b._3(0) && b._2 === RegNext(read_baddr) >> entIdxBits))
  val read_hit = read_hits.reduce(_||_)
  assert(!read_enable || PopCount(read_hits) <= 1.U)

  /* Get the read size */
  val read_size = Mux1H(read_hits, read_raw_size_tag_valid.map(_._1))
  val read_ents = Mux1H(read_hits, read_raw_ents)



  /* Create wires that are always connected to the memory write port */
  val write_baddr = WireDefault(UInt(baddrBits.W), DontCare)
  val write_size = WireDefault(UInt(lgMaxBBSize.W), DontCare)
  val write_ents = WireDefault(UInt(63.W), DontCare)
  val write_size_enable = WireDefault(false.B)
  val write_ents_enable = WireDefault(false.B)
  val write_enable = write_size_enable || write_ents_enable
  val write_mask = WireDefault(Vec(entNWays, Bool()), DontCare)
  val write_repl = WireDefault(false.B)

  /* Bundle the inputs */
  val write_raw_size_tag_valid = write_size ## (write_baddr >> entIdxBits) ## true.B

  /* If write_repl is true, then we need to perform random replacement */
  val write_random_way = LFSR(8, write_repl && write_enable)(log2Up(entNWays)-1, 0)
  val write_random_mask = VecInit(UIntToOH(write_random_way).asBools)
  val write_true_mask = Mux(write_repl, write_random_mask, write_mask)

  /* If we are doing random replacement, then we expect to write both a size and entangling sequence (or neither).
   * Otherwise we expect the mask to have exactly one bit set.
   */
  assert(!write_repl || (write_size_enable === write_ents_enable))
  assert(write_repl || !write_enable || PopCount(write_mask) === 1.U)

  /* Perform the write on the tag and size (if write_size_enable is true) */
  when(write_size_enable) {
    tag_size_array.write(write_baddr(entIdxBits-1, 0), Seq.fill(entNWays)(write_raw_size_tag_valid), write_true_mask)
  }

  /* Perform the write on the entanglings (if write_ents_enable is true) */
  when(write_ents_enable) {
    entangling_array.write(write_baddr(entIdxBits-1, 0), Seq.fill(entNWays)(write_ents), write_true_mask)
  }



  /* Create an encoder and decoder (we only need one of each, they can be shared by the states) */
  val encoder = Module(new EntanglingEncoder)
  val decoder = Module(new EntanglingDecoder)

  /* Default the encoder/decoder inputs and outputs */
  encoder.io.req.valid := false.B
  encoder.io.req.bits := DontCare
  decoder.io.req := DontCare



  /* Define the state transitions */
  switch(state) {

    /* Switch out of the ready state when there are requests to consume.
     * Prioritize prefetch requests.
     */
    is(s_ready) {
      /* When we can move into the prefetch state, read the size and entanglings array and move into the decode state */
      when(pref_q.valid) {
        readSizeAndEntanglings(pref_q.bits.baddr)
        pref_r := pref_q.deq() 
        state := s_pref_decode
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
        writeRandom(update_r.head, update_r.size)
      } .elsewhen(update_r.size > read_size) {
        writeAtWay(update_r.head, read_hits, Some(update_r.size))
      }

      /* Move into the ready state */
      state := s_ready
    }



    /* When we are in the entangle encode state, encode the entanglings and write to memory */
    is(s_entangle_encode) {
      /* If the read was a miss, then we don't want to entangle */
      when(!read_hit) {
        state := s_ready
      } .otherwise {
        /* Decode the entanglings. If the read was a miss, then we don't want to entangle. */
        decoder.io.req.head := entangle_r.src
        decoder.io.req.ents := read_ents

        /* Encode the entanglings */
        encoder.io.req.valid := true.B
        encoder.io.req.bits.head := entangle_r.src
        encoder.io.req.bits.baddrs := decoder.io.resp.baddrs.prepended(entangle_r.dst)
        encoder.io.req.bits.len := decoder.io.resp.len + 1.U

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
    is(s_pref_decode) {
      /* We want to move back into the ready state if the cache missed */
      when(!read_hit) {
        state := s_ready
      } .otherwise {
        io.pref_resp.valid := true.B
        io.pref_resp.bits.head := pref_r.baddr
        io.pref_resp.bits.size := read_size

        /* Decode the entanglings*/
        decoder.io.req.head := pref_r.baddr
        decoder.io.req.ents := read_ents

        /* If there are no dst entangled addresses then move to the ready state */
        when(decoder.io.resp.len === 0.U) {
          state := s_ready
        } .otherwise {
          /* Save the decoder output */
          pref_baddrs := decoder.io.resp.baddrs
          pref_len := decoder.io.resp.len - 1.U

          /* Initiate the first read */
          readSize(decoder.io.resp.baddrs(decoder.io.resp.len-1.U))

          /* Swap to the output state */
          state := s_pref_output
        }
      }
    }

    /* When in the prefetch output state, keep outputting entanglings */
    is(s_pref_output) {
      /* Respond with the basic block on a cache hit */
      when(read_hit) {
        io.pref_resp.valid := true.B
        io.pref_resp.bits.head := pref_baddrs(pref_len)
        io.pref_resp.bits.size := read_size
      }

      /* If there are no dst entangled addresses then move to the ready state */
      when(pref_len === 0.U) {
        state := s_ready
      } .otherwise {
        /* Decrement the length and trigger the next read */
        pref_len := pref_len - 1.U
        readSize(pref_baddrs(pref_len-1.U))
      }
    }

  }



  /** Trigger a read of the size of an address in the entangling table.
    *
    * @param baddr The address to search for.
    */
  def readSize(baddr: UInt) = {
    read_baddr := baddr
    read_size_enable := true.B
  }

  /** Trigger a read of the size of an address and its entanglings in the entangling table.
    *
    * @param baddr The address to search for.
    */
  def readSizeAndEntanglings(baddr: UInt) = {
    read_baddr := baddr
    read_size_enable := true.B
    read_ents_enable := true.B
  }

  /** Trigger a write of a size and/or entangling to the entangling table at a known way.
    *
    * @param baddr The address to write to.
    * @param mask The one-hot mask.
    * @param size Optionally, the size to write.
    * @param ents Optionally, the entangling sequence to write.
    */
  def writeAtWay(baddr: UInt, mask: Vec[Bool], size: Option[UInt], ents: Option[Bits] = None) = {
    write_baddr := baddr
    if(size.isDefined) {
      write_size := size.get
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
  def writeRandom(baddr: UInt, size: UInt, ents: Bits = 0.U) = {
    write_baddr := baddr
    write_size := size
    write_ents := ents
    write_size_enable := true.B
    write_ents_enable := true.B
    write_repl := true.B
  }

}



/** [[EntanglingIPrefetcherFetchReq]] defines the interface for notifying the prefetcher of a fetch.
  */ 
class EntanglingIPrefetcherFetchReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val paddr = UInt(paddrBits.W)
  val time = UInt(timeBits.W)
}

/** [[EntanglingIPrefetcherMissReq]] defines the interface for notifying the prefetcher of a cache miss.
 */
class EntanglingIPrefetcherMissReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val paddr = UInt(paddrBits.W)
  val start = UInt(timeBits.W)
  val end = UInt(timeBits.W)
}

/** [[EntanglingIPrefetcherPrefResp]] defines the interface for responding with prefetches.
 */
class EntanglingIPrefetcherPrefResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val paddr = UInt(paddrBits.W)
  val blocks = UInt(lgMaxBBSize.W)
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
    val pref_resp = Valid(new EntanglingIPrefetcherPrefResp)
  })



  /* Create the BB counter */
  val bb_counter = Module(new BBCounter)

  /* Connect its input IO */
  val fetch_baddr = io.fetch_req.bits.paddr >> blockOffBits
  bb_counter.io.req.valid := io.fetch_req.valid
  bb_counter.io.req.bits.baddr := fetch_baddr
  bb_counter.io.req.bits.time := io.fetch_req.bits.time



  /* Create the history buffer */
  val history_buffer = Module(new HistoryBuffer)

  /* Link up the insertion IO */
  history_buffer.io.insert_req.valid := bb_counter.io.resp.done
  history_buffer.io.insert_req.bits.head := bb_counter.io.resp.head
  history_buffer.io.insert_req.bits.time := bb_counter.io.resp.time

  /* Link up the search IO. We don't need to search if
   *  - the miss is invalid, or
   *  - the miss address isn't for this BB's head.
   */
  val miss_baddr = io.miss_req.bits.paddr >> blockOffBits
  history_buffer.io.search_req.valid := io.miss_req.valid && miss_baddr === bb_counter.io.resp.head
  history_buffer.io.search_req.bits.target_time := io.miss_req.bits.start - (io.miss_req.bits.end - io.miss_req.bits.start)



  /* Create the entangling table */
  val entangling_table = Module(new EntanglingTable)

  /* We don't want to keep asking the entangling table to look up the same baddr.
   * Only look up if we're extending the current basic block.
   */
  entangling_table.io.pref_req.valid := io.fetch_req.valid && (fetch_baddr < bb_counter.io.resp.head || fetch_baddr >= bb_counter.io.resp.head + bb_counter.io.resp.size)
  entangling_table.io.pref_req.bits.baddr := fetch_baddr

  /* Link up the new BB IO */
  entangling_table.io.update_req.valid := bb_counter.io.resp.done
  entangling_table.io.update_req.bits.head := bb_counter.io.resp.head
  entangling_table.io.update_req.bits.size := bb_counter.io.resp.size

  /* Link up the entangling creation IO */
  entangling_table.io.entangle_req.valid := history_buffer.io.search_resp.valid
  entangling_table.io.entangle_req.bits.src := history_buffer.io.search_resp.bits.head
  entangling_table.io.entangle_req.bits.dst := ShiftRegister(miss_baddr, histBufSearchLatency)

  /* Pipe the prefetch requests out of the prefetcher module */
  io.pref_resp.valid := entangling_table.io.pref_resp.valid
  io.pref_resp.bits.paddr := entangling_table.io.pref_resp.bits.head << blockOffBits
  io.pref_resp.bits.blocks := entangling_table.io.pref_resp.bits.size

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
    val test_baddrs_in = VecInit(baddrs.appendedAll(Seq.fill(7-baddrs.length)(0)).map(_.U(baddrBits.W)))

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
      for(i <- 0 until 6) {
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
  * @param queries A sequence of queries to make after delay cycles.
  * @param out A sequence of search responses to expect from the search requests.
  */
class HistoryBufferTest(id: Int, in: Seq[HistoryBufferInsertReq], delay: Int, queries: Seq[HistoryBufferSearchReq], out: Seq[Option[HistoryBufferSearchResp]])(implicit val p: Parameters) 
    extends UnitTest 
    with HasEntanglingIPrefetcherParameters 
{
  /* The tests must be within a UnitTestModule */
  class Impl extends UnitTestModule {
    
    /* Create the history buffer */
    val history_buffer = Module(new HistoryBuffer)

    /* Create ROM arrays for the sequences */
    assert(queries.length == out.length)
    val in_rom = VecInit(in)
    val queries_rom = VecInit(queries)
    val out_bits_rom = VecInit(out.map(_.getOrElse((new HistoryBufferSearchResp).Lit())))
    val out_valid_rom = VecInit(out.map(_.isDefined.B))

    /* Block until we have started */
    val started = RegEnable(io.start, false.B, io.start)

    /* Create registers to iterate over the inputs/outputs */
    def maxIterations = in.length.max(delay+queries.length+histBufSearchLatency)
    val i = RegInit(0.U(log2Up(maxIterations+1).W))
    val j = i - delay.U
    val k = j - histBufSearchLatency.U

    /* Iterate i */
    when(started && i =/= maxIterations.U) { i := i + 1.U }

    /* Wire up the insert requests */
    history_buffer.io.insert_req.valid := started && i < in.length.U
    history_buffer.io.insert_req.bits := in_rom(i)

    /* Wire up the search requests */
    history_buffer.io.search_req.valid := started && i >= delay.U && j < queries.length.U
    history_buffer.io.search_req.bits := queries_rom(j)

    /* Check the search results */
    when(started && i >= (delay+histBufSearchLatency).U && k < out.length.U) {
      assert(history_buffer.io.search_resp.valid === out_valid_rom(k),
        s"[HistoryBufferTest $id] i=%d j=%d k=%d expected output validity [%d], but got [%d]",
        i, j, k, out_valid_rom(k), history_buffer.io.search_resp.valid
      )
      assert(!out_valid_rom(k) || history_buffer.io.search_resp.bits === out_bits_rom(k),
        s"[HistoryBufferTest $id] i=%d j=%d k=%d expected result %x, but got %x",
        i, j, k, out_bits_rom(k).head, history_buffer.io.search_resp.bits.head
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
  prefReq: Seq[EntanglingTablePrefetchReq],
  prefResp: Seq[EntanglingTablePrefetchResp],
)(implicit val p: Parameters) extends UnitTest with HasEntanglingIPrefetcherParameters {
  /* The tests must be within a UnitTestModule */
  class Impl extends UnitTestModule {
    
    /* Create the entangling table */
    val entangling_table = Module(new EntanglingTable)

    /* Create ROMS for the test inputs/outputs (make sure none of them are empty) */
    val update_rom = VecInit(updateReq.appended((new EntanglingTableUpdateReq).Lit()))
    val entangle_rom = VecInit(entangleReq.appended((new EntanglingTableEntangleReq).Lit()))
    val pref_req_rom = VecInit(prefReq.appended((new EntanglingTablePrefetchReq).Lit()))
    val pref_resp_rom = VecInit(prefResp.appended((new EntanglingTablePrefetchResp).Lit()))

    /* Create a counter to time sending requests.
     * Assume each request can take a maximum of 8 clock cycles to complete.
     * This is a very high upper bound to be on the safe side.
     */
    val req_full_iter = RegInit(0.U(16.W))
    val req_iter = req_full_iter >> 3
    val req_fire = req_full_iter(2,0) === 0.U

    /* Indices for when to start each type of request */
    val start_entangle_req = updateReq.length
    val start_pref_req = start_entangle_req + entangleReq.length
    val end_pref_req = start_pref_req + prefReq.length
    val end_iteration = end_pref_req + 1

    /* Block until we have started */
    val started = RegEnable(io.start, false.B, io.start)

    /* Increment the iterator */
    when(started && req_iter < end_iteration.U) { req_full_iter := req_full_iter + 1.U }

    /* Make the update requests */
    entangling_table.io.update_req.valid := started && req_fire && req_iter < start_entangle_req.U
    entangling_table.io.update_req.bits := update_rom(req_iter)

    /* Make the entangle requests */
    entangling_table.io.entangle_req.valid := req_fire && req_iter >= start_entangle_req.U && req_iter < start_pref_req.U
    entangling_table.io.entangle_req.bits := entangle_rom(req_iter -& start_entangle_req.U)

    /* Make the prefetch requests */
    entangling_table.io.pref_req.valid := req_fire && req_iter >= start_pref_req.U && req_iter < end_pref_req.U
    entangling_table.io.pref_req.bits := pref_req_rom(req_iter -& start_pref_req.U)

    /* Check the results of prefetching */
    val resp_iter = RegInit(0.U(log2Up(prefResp.length+1).W))
    when(entangling_table.io.pref_resp.valid) {
      assert(resp_iter < prefResp.length.U,
        s"[EntanglingTableTest $id] extra prefetch request of {%x, %d}\n",
        entangling_table.io.pref_resp.bits.head, entangling_table.io.pref_resp.bits.size
      )
      assert(entangling_table.io.pref_resp.bits === pref_resp_rom(resp_iter),
        s"[EntanglingTableTest $id] output index %d mismatch: expected {%x, %d}, but got {%x, %d}\n",
        resp_iter, pref_resp_rom(resp_iter).head, pref_resp_rom(resp_iter).size,
        entangling_table.io.pref_resp.bits.head, entangling_table.io.pref_resp.bits.size 
      )
      resp_iter := resp_iter + 1.U
    }

    /* Check that we got all of the outputs */
    when(req_fire && req_iter === end_iteration.U) {
      assert(resp_iter === prefResp.length.U,
        s"[EntanglingTableTest $id] expected ${prefResp.length} responses, but got %d\n",
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