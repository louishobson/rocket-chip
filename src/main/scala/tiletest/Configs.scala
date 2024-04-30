// See LICENSE.Hobson for license details.

package freechips.rocketchip.tiletest

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util.log2Up
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._
import org.chipsalliance.cde.config._

class WithEntanglingTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    class Tests(implicit val p: Parameters) extends HasEntanglingIPrefetcherParameters {
      def midBaddr = 1 << (baddrBits-1)
      def produce = Seq(
        /* TEST 1: We can encode any (entanglingAddrBits/baddrBits) arbitrary addresses without compression */
        Module(new EntanglingTest(0, midBaddr, Seq.tabulate(entanglingAddrBits/baddrBits){case 0 => maxBaddr; case _ => 0}, 0)),

        /* TEST 2: We can encode maxEntanglings highly-compressed addresses */
        Module(new EntanglingTest(1, midBaddr, Seq.tabulate(maxEntanglings)(midBaddr+_), 0)),

        /* TEST 3: We should drop one address from a sequence of maxEntanglings+1, even when they are compressible */
        Module(new EntanglingTest(2, midBaddr, Seq.tabulate(maxEntanglings+1)(midBaddr+_), 1)),

        /* TEST 4: We should drop down to (entanglingAddrBits/baddrBits) address from (maxEntanglings+1) if none are compressible */
        Module(new EntanglingTest(3, midBaddr, (0 until maxEntanglings+1), maxEntanglings+1-entanglingAddrBits/baddrBits)),

        /* TEST 5: We can encode the empty sequence of baddrs */
        Module(new EntanglingTest(4, midBaddr, Seq(), 0)),
      )
    }
    new Tests()(q).produce ++ up.lift(UnitTests).map(_(q)).getOrElse(Seq())
  }
})

class WithBBCounterTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    class Tests(implicit val p: Parameters) extends HasEntanglingIPrefetcherParameters {
      def req(baddr: Int, timepidx: Int, valid: Boolean = true) = ((new BBCounterReq).Lit(_.baddr -> baddr.U, _.time -> timepidx.U, _.pidx -> timepidx.U), valid.B)
      def resp(head: Int, timepidx: Int, size: Int, done: Boolean) = (new BBCounterResp).Lit(_.head -> head.U, _.time -> timepidx.U, _.pidx -> timepidx.U, _.size -> size.U, _.done -> done.B)
      def produce = Seq(
        /* TEST 1: a sequence of consecutive addresses just increases the size of the BB */
        Module(new BBCounterTest(0, 
          Seq.tabulate(16)(i => req(i, i)),
          Seq.tabulate(16)(i => resp(0, 0, i+1, false))
        )),

        /* TEST 2: a sequence of increasing or equal addresses also just increases the size of the BB */
        Module(new BBCounterTest(1, 
          Seq.tabulate(16)(i => req((i/2).toInt, i)),
          Seq.tabulate(16)(i => resp(0, 0, (i/2).toInt+1, false))
        )),

        /* TEST 3: a sequence of addresses all in one BB, but non-increasing, increases the size of the BB when appropriate */
        Module(new BBCounterTest(2, 
          Seq(0, 1, 2, 3, 0, 1, 2, 3, 1, 2, 3, 4).map(i => req(i, i)),
          Seq(1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5).map(i => resp(0, 0, i, false))
        )),

        /* TEST 4: a new BB is started when the maximum BB size is exceeded */
        Module(new BBCounterTest(3, 
          Seq.tabulate(maxBBSize+1)(i => req(i, i)),
          Seq.tabulate(maxBBSize-1)(i => resp(0, 0, i+1, false))
            ++ Seq(resp(0, 0, maxBBSize, true), resp(maxBBSize, maxBBSize, 1, false))
        )),

        /* TEST 5: a jump forwards starts a new BB */
        Module(new BBCounterTest(4, 
          Seq(req(0, 0), req(1, 1), req(2, 2), req(4+maxBBGapSize, 3), req(5+maxBBGapSize, 4)),
          Seq(resp(0, 0, 1, false), resp(0, 0, 2, false), resp(0, 0, 3, true), resp(4+maxBBGapSize, 3, 1, false), resp(4+maxBBGapSize, 3, 2, false)),
        )),

        /* TEST 6: a jump backwards starts a new BB */
        Module(new BBCounterTest(5, 
          Seq(req(1, 0), req(2, 1), req(3, 2), req(0, 3), req(1, 4)),
          Seq(resp(1, 0, 1, false), resp(1, 0, 2, false), resp(1, 0, 3, true), resp(0, 3, 1, false), resp(0, 3, 2, false)),
        )),

        /* TEST 7: inputs marked as invalid don't cause a change in BB */
        Module(new BBCounterTest(6, 
          Seq(req(1, 0), req(2, 1), req(3, 2), req(0, 3, false), req(4, 4), req(6+maxBBGapSize, 5, false), req(5, 6)),
          Seq(resp(1, 0, 1, false), resp(1, 0, 2, false), resp(1, 0, 3, false), resp(1, 0, 3, false), resp(1, 0, 4, false), resp(1, 0, 4, false), resp(1, 0, 5, false)),
        )),

        /* TEST 8: we can have gaps of maxBBGapSize and stay within the same BB */
        Module(new BBCounterTest(7, 
          Seq(0, 1, 2, 3+maxBBGapSize, 4+maxBBGapSize, 5+maxBBGapSize).map(i => req(i, i)),
          Seq(1, 2, 3, 4+maxBBGapSize, 5+maxBBGapSize, 6+maxBBGapSize).map(i => resp(0, 0, i, false))
        )),
      )
    }
    new Tests()(q).produce ++ up.lift(UnitTests).map(_(q)).getOrElse(Seq())
  }
})

class WithHistoryBufferTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    class Tests(implicit val p: Parameters) extends HasEntanglingIPrefetcherParameters {
      def insertReq(head: Int, time: Int) = (new HistoryBufferInsertReq).Lit(_.head -> head.U, _.time -> time.U)
      def searchReq(dst: Int, time: Int) = (new HistoryBufferSearchReq).Lit(_.dst -> dst.U, _.target_time -> time.U)
      def searchResp(src: Int, dst: Int) = Some((new HistoryBufferSearchResp).Lit(_.src -> src.U, _.dst -> dst.U))
      def produce = Seq(
        /* TEST 1: We can search a non-full buffer for a positive result.
         * Insert {head: i, time: i*10} for i in [0, histBufLen-2).
         * After this, search for {dst: histBufLen, time: 15}.
         * We expect search i to return {src: 1, dst: 5).
         */
        Module(new HistoryBufferTest(0,
          Seq.tabulate(histBufLen-2)(i => insertReq(i, i*10)),
          searchReq(histBufLen, 15),
          searchResp(1, histBufLen)
        )),

        /* TEST 2: The search will fail when all timestamps in the history buffer are after the requested time.
         * Insert {head: i, time: (i+2)*10} for i in [0, histBufLen).
         * After this, search for {dst: 5, time: 15}.
         * The search should not yield any result.
         */
        Module(new HistoryBufferTest(1,
          Seq.tabulate(histBufLen)(i => insertReq(i, (i+2)*10)),
          searchReq(histBufLen, 15),
          None
        )),

        /* TEST 3: When the destination address is found in the history buffer before a valid source address,
         * then the search should not produce a result.
         * Insert {head: i, time: i} for i in [0, histBufLen).
         * After this, search for {dst: histBufLen-1, time: 0}.
         * There should be no response, as the destination address will be seen before or at the same
         * time as the entry with timestamp 0.
         */
        Module(new HistoryBufferTest(2,
          Seq.tabulate(histBufLen)(i => insertReq(i, i)),
          searchReq(histBufLen-1, 0),
          None
        ))
      )
    }
    new Tests()(q).produce ++ up.lift(UnitTests).map(_(q)).getOrElse(Seq())
  }
})

class WithEntanglingTableTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    class Tests(implicit val p: Parameters) extends HasEntanglingIPrefetcherParameters {
      def updateReq(head: Int, size: Int, pidx: Int) = (new EntanglingTableUpdateReq).Lit(_.head -> head.U, _.size -> size.U)
      def entangleReq(src: Int, dst: Int) = (new EntanglingTableEntangleReq).Lit(_.src -> src.U, _.dst -> dst.U)
      def prefetchReq(baddr: Int) = (new EntanglingTablePrefetchReq).Lit(_.baddr -> baddr.U)
      def prefetchResp(head: Int, size: Int, pidx: Int) = (new EntanglingTablePrefetchResp).Lit(_.head -> head.U, _.size -> size.U, _.pidx -> pidx.U)
      def produce = Seq(
        /* TEST 1: Just insert a single BB and request it as a prefetch. Expect the head to be incremented and the size to be decremented. */
        Module(new EntanglingTableTest(0,
          Seq(updateReq(1, 16, 1)), Seq(), Seq(prefetchReq(1)), Seq(prefetchResp(2, 15, 1))
        )),

        /* TEST 2: Just insert a BB, update its size, and then request it as a prefetch.
         * We expect that the the maximum size seen will be returned.
         */
        Module(new EntanglingTableTest(1,
          Seq(updateReq(1, 8, 1), updateReq(1, 16, 1), updateReq(1, 12, 1)), Seq(), Seq(prefetchReq(1)), Seq(prefetchResp(2, 15, 1))
        )),

        /* TEST 3: Write multiple baddrs and request both of them */
        Module(new EntanglingTableTest(2,
          Seq(updateReq(1, 8, 1), updateReq(2, 16, 2)), Seq(), Seq(prefetchReq(1), prefetchReq(2)), Seq(prefetchResp(2, 7, 1), prefetchResp(3, 15, 2))
        )),

        /* TEST 4: Fill the same way with many entries, and check that the last one prevailed */
        Module(new EntanglingTableTest(3,
          Seq.tabulate(128)(i => updateReq(i << entIdxBits, i % maxBBSize, i)).appended(updateReq(0, 16, 0)), 
          Seq(), Seq(prefetchReq(0)), Seq(prefetchResp(1, 15, 0))
        )),

        /* TEST 5: A cache miss results in no response */
        Module(new EntanglingTableTest(4,
          Seq(updateReq(0, 16, 0)), Seq(), Seq(prefetchReq(1)), Seq()
        )),

        /* TEST 6: We can create entanglings and get multiple responses when we request the source entangled address */
        Module(new EntanglingTableTest(5,
          Seq(updateReq(1, 16, 1), updateReq(2, 20, 2), updateReq(3, 24, 3), updateReq(4, 28, 4)), 
          Seq(entangleReq(1, 2), entangleReq(1, 3), entangleReq(1, 4)), 
          Seq(prefetchReq(1)), 
          Seq(prefetchResp(2, 15, 1), prefetchResp(2, 20, 2), prefetchResp(3, 24, 3), prefetchResp(4, 28, 4))
        )),

        /* TEST 7: Creating an entangling for a non-existent source address has no effect */
        Module(new EntanglingTableTest(6,
          Seq(), Seq(entangleReq(0, 16)), Seq(prefetchReq(0)), Seq()
        )),

        /* TEST 8: Where no entry for a dst entangled address exists, no prefetch should be issued */
        Module(new EntanglingTableTest(7,
          Seq(updateReq(1, 16, 1), updateReq(2, 20, 2), updateReq(4, 28, 3)), // Notice that no entry for baddr 3 is created 
          Seq(entangleReq(1, 2), entangleReq(1, 3), entangleReq(1, 4)),
          Seq(prefetchReq(1)), 
          Seq(prefetchResp(2, 15, 1), prefetchResp(2, 20, 2), prefetchResp(4, 28, 3)) // And we don't expect a prefetch response for baddr 3
        )),

        /* TEST 9: Entangling a dst baddr twice will only entangle it once */
        Module(new EntanglingTableTest(8,
          Seq(updateReq(1, 16, 1), updateReq(2, 20, 2)),
          Seq(entangleReq(1, 2), entangleReq(1, 2)),
          Seq(prefetchReq(1)), 
          Seq(prefetchResp(2, 15, 1), prefetchResp(2, 20, 2))
        )),

        /* TEST 10: Nothing will be emitted when the requested BB has size 1 */
        Module(new EntanglingTableTest(9,
          Seq(updateReq(1, 1, 1)),
          Seq(),
          Seq(prefetchReq(1)), 
          Seq()
        )),

        /* TEST 11: An update request replaces an old entry if the pidx is different,
         * but only if there are enough bits for a pidx to be required.
         */
        Module(new EntanglingTableTest(10,
          Seq(updateReq(1, 16, 0), updateReq(1, 12, 1)),
          Seq(),
          Seq(prefetchReq(1)), 
          Seq(if (pidxBits == 0) prefetchResp(2, 15, 3) else prefetchResp(2, 11, 1)) // The 3 is just proving the point that the pidx is not used
        )),
      )
    }
    new Tests()(q).produce ++ up.lift(UnitTests).map(_(q)).getOrElse(Seq())
  }
})

class EntanglingIPrefetcherUnitTestConfig extends Config(
  new WithEntanglingTableTests ++ 
  new WithHistoryBufferTests ++
  new WithBBCounterTests ++
  new WithEntanglingTests ++
  new WithEntanglingIPrefetcher ++
  new DefaultConfig
)

