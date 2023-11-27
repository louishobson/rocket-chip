package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{Cat, Valid, RegEnable, log2Up, isPow2}
import chisel3.util.random.LFSR
import chisel3.experimental.BundleLiterals._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.{DescribedSRAM}
import freechips.rocketchip.unittest._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.ShiftRegInit
import chisel3.util.ShiftRegister



/** [[Entangler]] defines helper methods to manipulate entanglings.
 */
class Entangler(baddrBits: Int) {

  /** Encode addresses to an entangling sequence.
    * This process will take at least one cycle, hence the validity bit input.
    * If the validity bit is set, then the inputs will override any currently encoding process.
    */ 
  def encode(len: UInt, baddrs: Vec[UInt], head: UInt, valid: Bool): (Bool, Bits) = {
    /* Create registers for each of the inputs */
    val len_reg = Reg(UInt(3.W))
    val baddrs_reg = Reg(VecInit(Seq.fill(baddrs.length)(UInt(baddrBits.W))))
    val head_reg = Reg(UInt(baddrBits.W))
    val valid_reg = RegInit(false.B)

    /* Create a register that fires once when an encoding completes */
    val result_done = RegInit(false.B)

    /* Create a register to store the output encoding */
    val result = Reg(UInt(63.W))

    /* When we receive a new input, overwrite any previous computation. */
    when(valid) {
      len_reg := len
      baddrs_reg := baddrs
      head_reg := head
      valid_reg := true.B
      result_done := false.B
    } 
    
    /* We don't have new input, so continue with the ongoing encoding, if one exists. */
    .elsewhen(valid_reg) {
      /* The number of bits each baddr will be compressed to */
      val entryBits = 60.U / len_reg

      /* Consider whether we can perform the compression. We must have
       *  - six or fewer addresses to entangle, and
       *  - each address must share the same MSBs as the head.
       */
      val baddrs_okay = len_reg <= 6.U && baddrs_reg.zipWithIndex.map{
        case (baddr, i) => (baddr >> entryBits) === (head_reg >> entryBits) || i.U > len_reg
      }.reduce(_||_)

      /* We can produce an output entangling sequence when baddrs_okay is flagged. */
      when(baddrs_okay) {
        valid_reg := false.B
        result_done := true.B
        result(62,60) := len_reg
        for(i <- 1 to 6) {
          when(i.U === len_reg) {
            result(59,0) := baddrs_reg.take(i).map(_(60/i-1,0)).reduce(_##_)
          }
        }
      } 
      
      /* Otherwise (baddrs_okay is false) we need to randomly pop one of the addresses */
      .otherwise {
        /* Move the final baddr to a random position, replacing the baddr previously in that position. */
        baddrs_reg(LFSR(16) % len_reg) := baddrs_reg(len_reg-1.U)
        len_reg := len_reg - 1.U
      }

    /* We only want result_done to fire for one cycle */
    } .otherwise {
      result_done := false.B
    }

    /* Return the result validity bit and the result itself */
    (result_done, result)
  }



  /** Decode the addresses in an entangling sequence.
    * The entire decode process is combinatorial logic.
    * 
    * @param ent_seq the entangling sequence (63 bits)
    * @param head the head of the BB this entangling is for 
    * @return A pair of the the number and vector of addresses
    */
  def decode(ent_seq: Bits, head: UInt): (UInt, Vec[UInt]) = {

    /* Get the mode of the entangling */
    val mode = ent_seq(62,60)

    /* Define the outputs as wires */
    val out = Wire(VecInit(Seq.fill(6)(UInt(baddrBits.W))))

    /* Iterate over each of the modes, and hardware branch over each iteration equalling the mode.
     * Note that this also works when the mode is 0, since the mode is part of the return pair,
     * and we don't care about the output vector in that case.
     */
    for (i <- 1 to 6) {
      when(i.U === mode) {

        /* This is the number of bits for each entangling entry in this mode */
        val entryBits = 60 / i

        /* Iterate over the number of addresses we need to extract */
        for (j <- 0 until i) {
          /* Detect whether the entire address fits in this entry, which makes setting the output easier */
          if (entryBits >= baddrBits) {
            out(j) := ent_seq(baddrBits+j*entryBits-1,j*entryBits)
          /* Else we need to reconstruct the address based on the head */
          } else {
            out(j) := head(baddrBits-1,entryBits) ## ent_seq((j+1)*entryBits-1,j*entryBits)
          }
        }
      }
    }

    /* Output the mode and the vector of addresses */
    (mode, out)
  }
}



/** [[EntanglingIPrefetcherFetchReq]] defines the interface for notifying the prefetcher of a fetch.
  */ 
class EntanglingIPrefetcherFetchReq(cfg: EntanglingIPrefetcherConfig)(implicit p: Parameters) extends CoreBundle()(p) {
  val paddr = UInt(paddrBits.W)
  val time = UInt(cfg.timeBits.W)
}

/** [[EntanglingIPrefetcherMissReq]] defines the interface for notifying the prefetcher of a cache miss.
 */
class EntanglingIPrefetcherMissReq(cfg: EntanglingIPrefetcherConfig)(implicit p: Parameters) extends CoreBundle()(p) {
  val paddr = UInt(paddrBits.W)
  val start = UInt(cfg.timeBits.W)
  val end = UInt(cfg.timeBits.W)
}

/** [[EntanglingIPrefetcherBundle]] defines the prefetcher IO.
  */ 
class EntanglingIPrefetcherBundle(cfg: EntanglingIPrefetcherConfig)(implicit p: Parameters) extends CoreBundle()(p) {
  val fetch_req = Flipped(Valid(new EntanglingIPrefetcherFetchReq(cfg)))
  val miss_req = Flipped(Valid(new EntanglingIPrefetcherMissReq(cfg)))
}



/** [[EntanglingIPrefetcherConfig]] defines configuration variables for the prefetcher.
  */
case class EntanglingIPrefetcherConfig(
  /* The bit width of timestamps */
  timeBits: Int = 64,
  /* the maximum size of a BB */
  maxBBSize: Int = 63,
  /* The length of the history buffer */
  histBufLen: Int = 8,
  /* The minimum size of a 'significant' BB */
  sigBBSize: Int = 2,
  /* The number of elements of the history buffer to search
   * in one combinatorial path.
   */
  histBufSearchFragLen: Int = 4,
  /* The ways and sets of the entangling table */
  nWays: Int = 4,
  nSets: Int = 1024,
)



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
class EntanglingIPrefetcher(cfg: EntanglingIPrefetcherConfig)(implicit p: Parameters) extends CoreModule()(p) with HasL1ICacheParameters {

  /** The io for the prefetcher */
  val io = IO(new EntanglingIPrefetcherBundle(cfg))



  /* The block address */
  val fetch_baddr = io.fetch_req.bits.paddr >> blockOffBits

  /* Initialise the BB head, size and timestamp registers */
  val bb_head = RegInit(0.U(baddrBits.W))
  val bb_size = RegInit(0.U(lgMaxBBSize.W))
  val bb_time = RegInit(0.U(cfg.timeBits.W))

  /* We want to remember whether this BB caused a cache miss,
   * so that we don't try to entangle it multiple times.
   */
  val bb_miss = RegInit(false.B)

  /* Calculate the change in fetch_baddr compared to the current head.
   * bb_cont indicates that we are within the same BB (true if baddr is not valid).
   * bb_next indicates that we have just extended the BB by one cache line (false if baddr is not valid).
   */
  val fetch_bdiff = fetch_baddr - bb_head
  val bb_cont = !io.fetch_req.valid || (fetch_bdiff <= bb_size && bb_size =/= cfg.maxBBSize.U)
  val bb_next = io.fetch_req.valid && fetch_bdiff === bb_size && bb_size =/= cfg.maxBBSize.U

  /* Update the registers */
  when(!bb_cont) { 
    bb_head := fetch_baddr 
    bb_time := io.fetch_req.bits.time
    bb_size := 1.U
    bb_miss := false.B
  } .elsewhen(bb_next) { 
    bb_size := bb_size + 1.U 
  }

  /* Recognize when a BB is significant: when a basic block reaches a size of sigBBSize.
   * This can stop tiny basic blocks from filling the entangling table.
   */
  require(cfg.sigBBSize >= 1, "sigBBSize must be >= 1")
  val newSigBB = !bb_cont && bb_size >= cfg.sigBBSize.U

  when(io.fetch_req.valid) {
    printf("## [%d] fetch_paddr %x; fetch_baddr %x; bb_cont %d; bb_next %d; bb_head %x; bb_size %d\n", io.fetch_req.bits.time, io.fetch_req.bits.paddr, fetch_baddr, bb_cont, bb_next, bb_head, bb_size)
  }



  /* Create the history buffer.
   * Setting the timestamps to the maximum value ensures they are not viewed
   * as a candidate for entangling before first assignment.
   */
  class HBBundle extends Bundle {
    val head = UInt(baddrBits.W)
    val time = UInt(cfg.timeBits.W)
  }
  val hist_buf = RegInit(VecInit(Seq.fill(cfg.histBufLen)((new HBBundle).Lit(_.time -> 0xFFFFFFFFl.U))))

  /* Insert a new significant block into the history buffer */
  when(newSigBB) {
    hist_buf(0).head := bb_head
    hist_buf(0).time := bb_time
    for (i <- 1 to cfg.histBufLen-1) {
      hist_buf(i).head := hist_buf(i-1).head
      hist_buf(i).time := hist_buf(i-1).time
    }
    for (i <- 0 to cfg.histBufLen-1) {
      printf(s"!! [$i] head=%x time=%d\n", hist_buf(i).head, hist_buf(i).time)
    }
  }



  /* Here, we want to search through the history buffer for the first entry with a timestamp
   * earlier than the target timestamp.
   * One option is to create a big combinatorial circuit across the entire buffer in order to 
   * find a match in a single clock cycle. However, this would be a very long combinatorial circuit.
   * Another option is to examine each entry in the history buffer in a separate cycle. Latency of the lookup
   * isn't a problem, but this would require a lot of extra registers.
   * To balance this we can mix the above: split the history buffer into groups of cfg.histBufSearchFragLen size,
   * and perform the search as a pipeline, taking one cycle to examine each group.
   * In order to do this, we will define a bundle which will store the state for each
   * stage of the search pipeline.
   */
  class HBSearchResultBundle extends Bundle {
    val head = UInt(baddrBits.W)
    val valid = Bool()
  }
  class HBSearchBundle extends Bundle {
    val req_baddr = UInt(baddrBits.W)
    val targ_time = UInt(cfg.timeBits.W)
    val result = new HBSearchResultBundle
    val valid = Bool()
  }

  /* Calculate the block address of the miss */
  val miss_baddr = io.miss_req.bits.paddr >> blockOffBits

  /* We don't need to search if
   *  - the miss is invalid,
   *  - the miss address isn't for this BB's head,
   *  - this BB's head already missed,
   *  - or we are about to continue on to a new BB (so the miss must have been superfluous),
   */
  val miss_valid = io.miss_req.valid && miss_baddr === bb_head && !bb_miss && bb_cont

  /* Update bb_miss appropriately. Note that this assignment to bb_miss is mutually exclusive 
   * to the other assignment to bb_miss based on bb_cont.
   */
  when(miss_valid) { bb_miss := true.B }

  /* Define the initial state of the search pipeline */
  val hb_search_init = RegInit((new HBSearchBundle).Lit(_.valid -> false.B))
  when(miss_valid) {
    hb_search_init.req_baddr := miss_baddr
    hb_search_init.targ_time := io.miss_req.bits.start - (io.miss_req.bits.end - io.miss_req.bits.start)
    hb_search_init.result.head := DontCare
    hb_search_init.result.valid := false.B
  }
  hb_search_init.valid := miss_valid
 
  /* Now group up the history buffer and fold over the stages of the pipeline */
  val hb_search_result = hist_buf.grouped(cfg.histBufSearchFragLen).foldLeft(hb_search_init)((prev, h) => {
    val next = RegInit((new HBSearchBundle).Lit(_.valid -> false.B))
    when(prev.valid) {
      next.req_baddr := prev.req_baddr
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



  /* Define the BB tag and size SRAM */
  class EntTagBundle extends Bundle {
    val tag = UInt(entTagBits.W)
    val size = UInt(lgMaxBBSize.W)
  }
  val ent_tag_size_array = DescribedSRAM(
    name = "tag_and_size_array",
    desc = "ICache Tag Array",
    size = cfg.nSets,
    data = Vec(cfg.nWays, new EntTagBundle)
  )



  val hb_search_result_good = hb_search_result.valid && hb_search_result.result.valid && hb_search_result.req_baddr === bb_head && bb_cont
  val ent_target_tags = ent_tag_size_array.read(hb_search_result.result.head(entIdxBits,0), hb_search_result_good)




  /* The log maximum BB size */
  def lgMaxBBSize = log2Up(cfg.maxBBSize + 1).toInt

  /* The number of bits for a block address */
  def baddrBits = paddrBits - blockOffBits

  /* The number if tag bits for the entangling table */
  require(isPow2(cfg.nSets), "cfg.nSets must be a power of 2")
  def entIdxBits = log2Up(cfg.nSets)
  def entTagBits = baddrBits - entIdxBits

}



/** [[EntanglerTest]] instruments tests for block address compression. */
class EntanglerTest extends UnitTest {
  /* The tests must be within a UnitTestModule */
  class Impl extends UnitTestModule {
    io.finished := ShiftRegInit(true.B, 40, false.B)
  }

  /* Instantiate the test module */
  val dut = Module(new Impl)

  /* Connect the UnitTest IO to the UnitTestModule IO */
  dut.io.start := io.start 
  io.finished := dut.io.finished
}