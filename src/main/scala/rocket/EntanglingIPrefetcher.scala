package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{Cat, Valid}
import chisel3.experimental.BundleLiterals._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config.Parameters
import chisel3.util.log2Up
import chisel3.util.log2Ceil
import chisel3.util.log2Floor
import chisel3.util.RegEnable
import chisel3.util.MuxLookup



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
  /* The length of the history buffer */
  histBufLen: Int = 8,
  /* The minimum size of a 'significant' BB */
  sigBBSize: Int = 2,
  /* The number of elements of the history buffer to search
   * in one combinatorial path.
   */
  histBufSearchFragLen: Int = 4
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
  val bb_size = RegInit(0.U(8.W))
  val bb_time = RegInit(0.U(cfg.timeBits.W))

  /* We also want to remember whether this BB caused a cache miss,
   * so that we don't try to entangle it multiple times.
   */
  val bb_miss = RegInit(false.B)

  /* 
   */
  val bb_entangle = RegInit(0.U(baddrBits.W))

  /* Calculate the change in fetch_baddr compared to the current head.
   * bb_cont indicates that we are within the same BB (true if baddr is not valid).
   * bb_next indicates that we have just extended the BB by one cache line (false if baddr is not valid).
   */
  val fetch_bdiff = fetch_baddr - bb_head
  val bb_cont = !io.fetch_req.valid || fetch_bdiff <= bb_size
  val bb_next = io.fetch_req.valid && fetch_bdiff === bb_size

  /* Update the registers */
  when(!bb_cont) { 
    bb_head := fetch_baddr 
    bb_time := io.fetch_req.bits.time
    bb_size := 1.U
    bb_miss := false.B
  } .elsewhen(bb_next) { 
    bb_size := bb_size + 1.U 
  }

  when(io.fetch_req.valid) {
    printf("## [%d] fetch_paddr %x; fetch_baddr %x; bb_cont %d; bb_next %d; bb_head %x; bb_size %d\n", io.fetch_req.bits.time, io.fetch_req.bits.paddr, fetch_baddr, bb_cont, bb_next, bb_head, bb_size)
  }



  /* Create the history buffer.
   * Setting the timestamps to the maximum value ensures they are not viewed
   * as a candidate for entangling before first assignment.
   */
  class HistBufBundle extends Bundle {
    val head = UInt(baddrBits.W)
    val time = UInt(cfg.timeBits.W)
  }
  val hist_buf = RegInit(VecInit(Seq.fill(cfg.histBufLen)((new HistBufBundle).Lit(_.time -> 0xFFFFFFFFl.U))))



  /* Recognize when a BB is significant: when a basic block reaches a size of sigBBSize.
   * This can stop tiny basic blocks from filling the entangling table.
   */
  require(cfg.sigBBSize >= 1, "sigBBSize must be >= 1")
  val newSigBB = !bb_cont && bb_size >= (cfg.sigBBSize-1).U

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



  /** The number of bits for a block address */
  def baddrBits = paddrBits - blockOffBits
    



