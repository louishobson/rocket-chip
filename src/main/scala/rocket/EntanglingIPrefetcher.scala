package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{Cat, Decoupled, Enum, RegEnable, Valid, log2Up, isPow2}
import chisel3.util.random.LFSR
import chisel3.experimental.BundleLiterals._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.{DescribedSRAM}
import freechips.rocketchip.unittest._
import freechips.rocketchip.diplomacy._



/** [[EntanglingIPrefetcherConfig]] defines configuration variables for the prefetcher.
  */
case class EntanglingIPrefetcherParams(
  /* The bit width of timestamps */
  timeBits: Int = 64,
  /* the maximum size of a BB */
  maxBBSize: Int = 63,
  /* The length of the history buffer */
  histBufLen: Int = 8,
  /* The minimum size of a 'significant' BB */
  sigBBSize: Int = 2,
  /* The number of elements of the history buffer to search in one combinatorial path */
  histBufSearchFragLen: Int = 4,
  /* The ways and sets of the entangling table */
  nWays: Int = 4,
  nSets: Int = 1024,
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
  def histBufSearchFragLen = entanglingParams.histBufSearchFragLen
  def entanglingNWays = entanglingParams.nWays
  def entanglingNSets = entanglingParams.nSets

  /* The block address size */
  def baddrBits = paddrBits - blockOffBits

  /* Configuration for the entangling table */
  require(isPow2(nWays), "entanglingNSets must be a power of 2")
  def entIdxBits = log2Up(nSets)
  def entTagBits = baddrBits - entIdxBits

  /* The number of bits required to store a BB size */
  def lgMaxBBSize = log2Up(maxBBSize + 1).toInt

  /* the history buffer search latency */
  def histBufSearchLatency = Math.ceil(histBufLen/histBufSearchFragLen).toInt + 1

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
  val ent = Bits(63.W)
}

/** [[EntanglingEncoder]] compresses baddrs into a single 63-bit value.
  * The process can take a varying number of cycles depending on how many addresses need to be dropped.
  * For this reason, the IO is decoupled. 
  *
  * @param keepLastBaddr Whether the last baddr in the input sequence should be dropped by random replacement.
  * This is useful when a newly-added baddr is always the last in the input sequence. 
  */
class EntanglingEncoder(keepLastBaddr: Boolean = true)(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO. The encoding process can take multiple cycles, so use decoupled IO. */
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new EntanglerEncodeReq(7)))
    val resp = Decoupled(new EntanglerEncodeResp)
  })

  /* Create registers for each of the inputs */
  val len_reg = Reg(UInt(3.W))
  val baddrs_reg = Reg(Vec(7, UInt(baddrBits.W)))
  val head_reg = Reg(UInt(baddrBits.W))

  /* Whether the encoder is currently busy */
  val busy = RegInit(false.B)

  /* Create a register that fires once when an encoding completes */
  val done = RegInit(false.B)

  /* Create a register to store the output encoding */
  val ent = Reg(UInt(63.W))

  /* We may be both 'busy' and done. In this case we are waiting for the requestor to be ready. */
  when(busy && done) {
    busy := !io.resp.ready
  }
  
  /* Otherwise if we are busy and not done, then continue with the ongoing encoding */
  .elsewhen(busy) {
    /* The number of bits each baddr will be compressed to.
     * Division by 0 is fine here, as entryBits won't actually be used in that case.
     */
    val entryBits = 60.U / len_reg

    /* Consider whether we can perform the compression. We must have
     *  - six or fewer addresses to entangle, and
     *  - each address must share the same MSBs as the head.
     */
    val baddrs_okay = len_reg <= 6.U && baddrs_reg.zipWithIndex.map{
      case (baddr, i) => (baddr >> entryBits) === (head_reg >> entryBits) || i.U >= len_reg
    }.reduce(_&&_)

    /* We can produce an output entangling sequence when baddrs_okay is flagged.
     * Ent is initialized to 0, which is the correct output for the case where len_reg is 0.
     */
    when(baddrs_okay) {
      done := true.B
      for(i <- 1 to 6) {
        when(i.U === len_reg) {
          ent := len_reg ## baddrs_reg.take(i).map(WireInit(UInt((60/i).W), _)).reduce(_##_)
        }
      }
    } 
    
    /* Otherwise (baddrs_okay is false) we need to randomly pop one of the addresses */
    .otherwise {
      /* Generate a random number for which index is to be dropped.
       * This should never happen while len_reg is <= 1.
       */
      assert(len_reg > 1.U)
      val rnd = LFSR(8) % (if (keepLastBaddr) len_reg - 1.U else len_reg)

      /* Shift all registers down, but only if the index is geq the random number */
      for(i <- 0 to 5) {
        when(i.U >= rnd) {
          baddrs_reg(i) := baddrs_reg(i+1)
        }
      }
      
      /* Decrement the length register */
      len_reg := len_reg - 1.U
    }
  }

  /* Otherwise if we are not busy and have valid input, consume it */
  .elsewhen(io.req.valid) {
    len_reg := io.req.bits.len
    baddrs_reg := io.req.bits.baddrs
    head_reg := io.req.bits.head
    busy := true.B
    done := false.B
    ent := 0.U
  }

  /* We are not busy and there is not another request: set done to false for the next cycle */
  .otherwise {
    done := false.B
  }

  /* We want to inform the user whether we are busy or not */
  io.req.ready := !busy

  /* Set the output ports */
  io.resp.bits.head := head_reg
  io.resp.bits.ent := ent
  io.resp.valid := done
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
  val mode = io.req.ent(62,60)

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
          io.resp.baddrs(j) := io.req.ent(baddrBits+j*entryBits-1,j*entryBits)
        /* Else we need to reconstruct the address based on the head */
        } else {
          io.resp.baddrs(j) := io.req.head(baddrBits-1,entryBits) ## io.req.ent((j+1)*entryBits-1,j*entryBits)
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
   * bb_cont indicates that we are within the same BB (true if baddr is not valid).
   * bb_next indicates that we have just extended the BB by one cache line (false if baddr is not valid).
   */
  val req_bdiff = io.req.bits.baddr - bb_head
  val bb_cont = !io.req.valid || (req_bdiff <= bb_size && bb_size =/= maxBBSize.U)
  val bb_next = io.req.valid && req_bdiff === bb_size && bb_size =/= maxBBSize.U

  /* Update the registers */
  when(!bb_cont) { 
    bb_head := io.req.bits.baddr 
    bb_time := io.req.bits.time 
    bb_size := 1.U
  } .elsewhen(bb_next) { 
    bb_size := bb_size + 1.U 
  }

  require(sigBBSize >= 1, "sigBBSize must be >= 1")
  val newSigBB = 

  /* Print on new inputs */
  when(io.req.valid) {
    //printf("## [%d] io.req.bits.baddr %x; bb_cont %d; bb_next %d; bb_head %x; bb_size %d\n", io.req.bits.time, io.req.bits.baddr, bb_cont, bb_next, bb_head, bb_size)
  }

  /* Potentially output a completed significant BB. This is when a basic block reaches a size of sigBBSize.
   * This can stop tiny basic blocks from filling the entangling table.
   */
  io.resp.done := !bb_cont && bb_size >= sigBBSize.U
  io.resp.head := bb_head
  io.resp.size := bb_size
  io.resp.time := bb_time

}



/** [[HBInsertReq]] defines the interface for requesting an insert operation on the HB.
  */ 
class HBInsertReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val time = UInt(timeBits.W)
}

/** [[HBSearchReq]] defines the interface for requesting a search of the HB.
  */
class HBSearchReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val targ_time = UInt(timeBits.W)
}

/** [[HBSearchResp]] defines the interface for responding to a HB search request.
  * If the search is unsuccessful, then no response is made. 
  */
class HBSearchResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
}

/** [[HistoryBuffer]] implements a history buffer, with BB heads labelled with timestamps.
  */
class HistoryBuffer(implicit p: Parameters) extends CoreModule with HasEntanglingIPrefetcherParameters {

  /* Define the IO */
  val io = IO(new Bundle{
    val insert_req = Flipped(Valid(new HBInsertReq))
    val search_req = Flipped(Valid(new HBSearchReq))
    val search_resp = Valid(new HBSearchResp)
  })

  /* Create the history buffer.
   * Setting the timestamps to the maximum value ensures they are not viewed
   * as a candidate for entangling before first assignment.
   */
  class HBBundle extends Bundle {
    val head = UInt(baddrBits.W)
    val time = UInt(timeBits.W)
  }
  val hist_buf = RegInit(VecInit(Seq.fill(histBufLen)((new HBBundle).Lit(_.time -> 0xFFFFFFFFl.U))))

  /* Insert a new significant block into the history buffer */
  when(io.insert_req.valid) {
    hist_buf(0).head := io.insert_req.bits.head
    hist_buf(0).time := io.insert_req.bits.time
    for (i <- 1 to histBufLen-1) {
      hist_buf(i).head := hist_buf(i-1).head
      hist_buf(i).time := hist_buf(i-1).time
    }
    for (i <- 0 to histBufLen-1) {
      //printf(s"!! [$i] head=%x time=%d\n", hist_buf(i).head, hist_buf(i).time)
    }
    //printf("\n")
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
    val targ_time = UInt(timeBits.W)
    val result = new HBSearchResultBundle
    val valid = Bool()
  }

  /* Define the initial state of the search pipeline */
  val hb_search_init = RegInit((new HBSearchBundle).Lit(_.valid -> false.B))
  when(io.search_req.valid) {
    hb_search_init.targ_time := io.search_req.bits.targ_time
    hb_search_init.result.head := DontCare
    hb_search_init.result.valid := false.B
  }
  hb_search_init.valid := io.search_req.valid
 
  /* Now group up the history buffer and fold over the stages of the pipeline */
  val hb_search_result = hist_buf.grouped(histBufSearchFragLen).foldLeft(hb_search_init)((prev, h) => {
    val next = RegInit((new HBSearchBundle).Lit(_.valid -> false.B))
    when(prev.valid) {
      next.targ_time := prev.targ_time
      next.result := h.foldLeft(prev.result)((p, x) => {
        val w = Wire(new HBSearchResultBundle)
        when (!p.valid && x.time <= prev.targ_time) { 
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



/** [[EntanglingTableBBNotifyReq]] defines the interface for notifying the entangling table that a new basic block has started.
  */ 
class EntanglingTablePrefReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val baddr = UInt(baddrBits.W)
}

/** [[EntanglingTablePrefResp]] defines the interface for responding with BBs that should be prefetched.
  */ 
class EntanglingTablePrefResp(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
  val head = UInt(baddrBits.W)
  val size = UInt(lgMaxBBSize.W)
}

/** [[EntanglingTableBBUpdateReq]] defines the interface for updating a basic block size.
  */ 
class EntanglingTableBBUpdateReq(implicit p: Parameters) extends CoreBundle with HasEntanglingIPrefetcherParameters {
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
    val pref_req = Flipped(Decoupled(new EntanglingTablePrefReq))
    val bb_update_req = Flipped(Decoupled(new EntanglingTableBBUpdateReq))
    val entangle_req = Flipped(Decoupled(new EntanglingTableEntangleReq))
    val pref_resp = Valid(new EntanglingTablePrefResp)
  })

  /* Define the state register */
  val s_ready :: s_pref :: s_update :: s_entangle :: Nil = Enum(4)
  val state = RegInit(s_ready)

  

  /* Define the tag and size SRAM */
  class TagSizeBundle extends Bundle {
    val tag = UInt(entTagBits.W)
    val size = UInt(lgMaxBBSize.W)
    val valid = Bool()
  }
  val tag_size_array = DescribedSRAM(
    name = "tag_and_size_array",
    desc = "Entangling Prefetcher Tag and Size Array",
    size = entanglingNSets,
    data = Vec(entanglingNWays, new TagSizeBundle)
  )

  /* Define the entangling SRAM */
  val entangling_array = DescribedSRAM(
    name = "entangling_array",
    desc = "Entangling Prefetcher Entangling Array",
    size = entanglingNSets,
    data = Vec(entanglingNWays, UInt(63.W))
  ) 

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

/** [[EntanglingIPrefetcherMissReq]] defines the interface for notifying the prefetcher of a cache miss.
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
  })



  /* Create the BB counter */
  val bb_counter = Module(new BBCounter)

  /* Connect its input IO */
  bb_counter.io.req.valid := io.fetch_req.valid
  bb_counter.io.req.bits.baddr := io.fetch_req.bits.paddr >> blockOffBits
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
  history_buffer.io.search_req.bits.targ_time := io.miss_req.bits.start - (io.miss_req.bits.end - io.miss_req.bits.start)

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
  *   - and the last baddr in the input sequence is present in the output sequence.
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

    /* Create registers for the inputs */
    val test_len_in = RegInit(baddrs.length.U)
    val test_baddrs_in = RegInit(VecInit(baddrs.appendedAll(Seq.fill(7-baddrs.length)(0)).map(_.U(baddrBits.W))))
    val test_head = RegInit(head.U(baddrBits.W))
    val test_valid_in = RegNext(io.start, false.B)

    /* Perform the encoding */
    encoder.io.req.bits.len := test_len_in
    encoder.io.req.bits.baddrs := test_baddrs_in
    encoder.io.req.bits.head := test_head
    encoder.io.req.valid := test_valid_in

    /* Perform the decoding */
    encoder.io.resp.ready := true.B
    decoder.io.req.ent := encoder.io.resp.bits.ent
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
      assert(test_len_out === test_len_in - exp_drops.U, 
        s"[EntanglerTest $id]: test_len_in: %d, test_len_out: %d, exp_drops: $exp_drops\n", 
        test_len_in, test_len_out
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

      /* Check that the last input address is present in the output addresses */
      when(test_len_in > 0.U) {
        assert(test_baddrs_out.contains(test_baddrs_in(test_len_in-1.U)),
          s"[EntanglerTest $id] random replacement dropped the last baddr!", 
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



/** [[BasicBlockTest]] instruments tests for basic block accumulation. 
  *
  * @param id The ID of the test (for debugging purposes).
  * @param in The sequence of requests to make to the BB counter, as well as a validity flag.
  * @param out The sequence of responses to expect from the BB counter. The corresponding response for 
  * a request is checked the cycle after that request is made. 
  */
class BasicBlockTest(id: Int, in: Seq[(BBCounterReq, Bool)], out: Seq[BBCounterResp])(implicit val p: Parameters) 
    extends UnitTest 
    with HasEntanglingIPrefetcherParameters 
{
  /* The tests must be within a UnitTestModule */
  class Impl extends UnitTestModule {

    /* Create the BB counter */
    val bb_counter = Module(new BBCounter)

    /* Create a register array for the test sequences */
    assert(in.length == out.length)
    val in_bits_reg = RegInit(VecInit(in.map(_._1)))
    val in_valid_reg = RegInit(VecInit(in.map(_._2)))
    val out_reg = RegInit(VecInit(out))

    /* Block until we have started */
    val started = RegEnable(io.start, false.B, io.start)
    
    /* Create an iterator for passing through the inputs and outputs */
    val i = RegInit(0.U(log2Up(in.length+2).W))
    when(started && i < (in.length+1).U) { i := i + 1.U }

    /* Pass-in the inputs */
    bb_counter.io.req.valid := started && i < in.length.U && in_valid_reg(i)
    bb_counter.io.req.bits := in_bits_reg(i)

    /* Check the outputs */
    when(i > 0.U && i <= out.length.U) {
      val j = i-1.U // if I use (i-1.U) directly for the below, then i is coerced to a smaller number of bits before subtracting one
      assert(bb_counter.io.resp === out_reg(j), 
        s"[BasicBlockTest $id] i=%d expected {%x, %d, %d, %d}, but got {%x, %d, %d, %d}",
        i, out_reg(j).head, out_reg(j).time, out_reg(j).size, out_reg(j).done,
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
class HistoryBufferTest(id: Int, in: Seq[HBInsertReq], delay: Int, queries: Seq[HBSearchReq], out: Seq[Option[HBSearchResp]])(implicit val p: Parameters) 
    extends UnitTest 
    with HasEntanglingIPrefetcherParameters 
{
  /* The tests must be within a UnitTestModule */
  class Impl extends UnitTestModule {
    
    /* Create the history buffer */
    val history_buffer = Module(new HistoryBuffer)

    /* Create a register array for the sequences */
    assert(queries.length == out.length)
    val in_reg = RegInit(VecInit(in))
    val queries_reg = RegInit(VecInit(queries))
    val out_bits_reg = RegInit(VecInit(out.map(_.getOrElse((new HBSearchResp).Lit()))))
    val out_valid_reg = RegInit(VecInit(out.map(_.isDefined.B)))

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
    history_buffer.io.insert_req.bits := in_reg(i)

    /* Wire up the search requests */
    history_buffer.io.search_req.valid := started && i >= delay.U && j < queries.length.U
    history_buffer.io.search_req.bits := queries_reg(j)

    /* Check the search results */
    when(started && i >= (delay+histBufSearchLatency).U && k < out.length.U) {
      assert(history_buffer.io.search_resp.valid === out_valid_reg(k),
        s"[HistoryBufferTest $id] i=%d j=%d k=%d expected output validity %d, but got %d",
        i, j, k, out_valid_reg(k), history_buffer.io.search_resp.valid
      )
      assert(!out_valid_reg(k) || history_buffer.io.search_resp.bits === out_bits_reg(k),
        s"[HistoryBufferTest $id] i=%d j=%d k=%d expected result %x, but got %x",
        i, j, k, out_bits_reg(k).head, history_buffer.io.search_resp.bits.head
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