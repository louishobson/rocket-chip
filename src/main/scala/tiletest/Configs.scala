package freechips.rocketchip.tiletest

import chisel3._
import freechips.rocketchip.rocket._
import freechips.rocketchip.system._
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._
import org.chipsalliance.cde.config._
import chisel3.experimental.BundleLiterals._
import chisel3.util.log2Up

class WithEntanglingTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    class Tests(implicit val p: Parameters) extends HasEntanglingIPrefetcherParameters {
      def midBaddr = 1 << (baddrBits-1)
      def produce = Seq(
        /* TEST 1: We can encode any two arbitrary addresses without compression */
        Module(new EntanglingTest(0, midBaddr, Seq(0, maxBaddr), 0)),

        /* TEST 2: We can encode six highly-compressed addresses */
        Module(new EntanglingTest(1, midBaddr, Seq.tabulate(6)(midBaddr+_), 0)),

        /* TEST 3: We should drop one address from a sequence of 7, even when they are compressible */
        Module(new EntanglingTest(2, midBaddr, Seq.tabulate(7)(midBaddr+_), 1)),

        /* TEST 4: We should drop down to two address if none are compressible */
        Module(new EntanglingTest(3, midBaddr, (0 until 7), 5)),

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
      def req(baddr: Int, time: Int, valid: Boolean = true) = ((new BBCounterReq).Lit(_.baddr -> baddr.U, _.time -> time.U), valid.B)
      def resp(head: Int, time: Int, size: Int, done: Boolean) = (new BBCounterResp).Lit(_.head -> head.U, _.time -> time.U, _.size -> size.U, _.done -> done.B)
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
      def searchReq(time: Int) = (new HistoryBufferSearchReq).Lit(_.targ_time -> time.U)
      def searchResp(head: Int) = Some((new HistoryBufferSearchResp).Lit(_.head -> head.U))
      def produce = Seq(
        /* TEST 1: We can search a non-full buffer for a positive result.
         * Insert (head: i, time: i*10) for i in [0, histBufLen-2).
         * After this, search for (time: i*10+5) for i in [0, histBufLen).
         * We expect search i to return (head: min(i, histBufLen-3)).
         */
        Module(new HistoryBufferTest(0,
          Seq.tabulate(histBufLen-2)(i => insertReq(i, i*10)),
          histBufLen-2,
          Seq.tabulate(histBufLen)(i => searchReq(i*10+5)),
          Seq.tabulate(histBufLen)(i => searchResp(i.min(histBufLen-3)))
        )),

        /* TEST 2: We can search a the buffer and start receiving positive results
         * when our search time enters the range contained in the buffer.
         * Insert (head: i+2, time: (i+2)*10) for i in [0, histBufLen).
         * After this, search for (time: i*10+5) for i in [0, histBufLen).
         * The first two searches should fail, but the rest should succeed with (head: i).
         */
        Module(new HistoryBufferTest(1,
          Seq.tabulate(histBufLen)(i => insertReq(i+2, (i+2)*10)),
          histBufLen,
          Seq.tabulate(histBufLen)(i => searchReq(i*10+5)),
          Seq(None, None) ++ Seq.tabulate(histBufLen-2)(i => searchResp(i+2))
        )),

        /* TEST 3: Ensure that we can search while simultaneously inserting.
         * Insert (head: i, time: i) for i in [0, histBufLen*2).
         * After histBufLen cycles the whole buffer is full.
         * At this point, send histBufLen search requests for (time: histBufLen-1).
         * This will succeed for the first (histBufLen - (histBufSearchLatency-1)) cycles.
         * But after that, (head: histBufLen-1, time: histBufLen-1) will be evicted before being found by the search.
         */
        Module(new HistoryBufferTest(2,
          Seq.tabulate(histBufLen*2)(i => insertReq(i, i)),
          histBufLen,
          Seq.tabulate(histBufLen)(i => searchReq(histBufLen-1)),
          Seq.fill(histBufLen-(histBufSearchLatency-1))(searchResp(histBufLen-1)) 
            ++ Seq.fill(histBufSearchLatency-1)(None)
        ))
      )
    }
    new Tests()(q).produce ++ up.lift(UnitTests).map(_(q)).getOrElse(Seq())
  }
})

class WithEntanglingTableTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    class Tests(implicit val p: Parameters) extends HasEntanglingIPrefetcherParameters {
      def updateReq(head: Int, size: Int) = (new EntanglingTableUpdateReq).Lit(_.head -> head.U, _.size -> size.U)
      def entangleReq(src: Int, dst: Int) = (new EntanglingTableEntangleReq).Lit(_.src -> src.U, _.dst -> dst.U)
      def prefetchReq(baddr: Int) = (new EntanglingTablePrefetchReq).Lit(_.baddr -> baddr.U)
      def prefetchResp(head: Int, size: Int) = (new EntanglingTablePrefetchResp).Lit(_.head -> head.U, _.size -> size.U)
      def produce = Seq(
        /* TEST 1: Just insert a single BB and request it as a prefetch */
        Module(new EntanglingTableTest(0,
          Seq(updateReq(1, 16)), Seq(), Seq(prefetchReq(1)), Seq(prefetchResp(1, 16))
        )),

        /* TEST 2: Just insert a BB, update its size, and then request it as a prefetch.
         * We expect that the the maximum size seen will be returned.
         */
        Module(new EntanglingTableTest(1,
          Seq(updateReq(1, 8), updateReq(1, 16), updateReq(1, 12)), Seq(), Seq(prefetchReq(1)), Seq(prefetchResp(1, 16))
        )),

        /* TEST 3: Write multiple baddrs and request both of them */
        Module(new EntanglingTableTest(2,
          Seq(updateReq(1, 8), updateReq(2, 16)), Seq(), Seq(prefetchReq(1), prefetchReq(2)), Seq(prefetchResp(1, 8), prefetchResp(2, 16))
        )),

        /* TEST 4: Fill the same way with many entries, and check that the last one prevailed */
        Module(new EntanglingTableTest(3,
          Seq.tabulate(128)(i => updateReq(i << entIdxBits, i % maxBBSize)).appended(updateReq(0, 16)), 
          Seq(), Seq(prefetchReq(0)), Seq(prefetchResp(0, 16))
        )),

        /* TEST 5: A cache miss results in no response */
        Module(new EntanglingTableTest(4,
          Seq(updateReq(0, 16)), Seq(), Seq(prefetchReq(1)), Seq()
        )),

        /* TEST 6: We can create entanglings and get multiple responses when we request the source entangled address */
        Module(new EntanglingTableTest(5,
          Seq(updateReq(1, 16), updateReq(2, 20), updateReq(3, 24), updateReq(4, 28)), 
          Seq(entangleReq(1, 2), entangleReq(1, 3), entangleReq(1, 4)), 
          Seq(prefetchReq(1)), 
          Seq(prefetchResp(1, 16), prefetchResp(2, 20), prefetchResp(3, 24), prefetchResp(4, 28))
        )),

        /* TEST 7: Creating an entangling for a non-existent source address has no effect */
        Module(new EntanglingTableTest(6,
          Seq(), Seq(entangleReq(0, 1)), Seq(prefetchReq(0)), Seq()
        )),

        /* TEST 8: Where no entry for a dst entangled address exists, no prefetch should be issued */
        Module(new EntanglingTableTest(7,
          Seq(updateReq(1, 16), updateReq(2, 20), updateReq(4, 28)), // Notice that no entry for baddr 3 is created 
          Seq(entangleReq(1, 2), entangleReq(1, 3), entangleReq(1, 4)),
          Seq(prefetchReq(1)), 
          Seq(prefetchResp(1, 16), prefetchResp(2, 20), prefetchResp(4, 28)) // And we don't expect a prefetch response for baddr 3
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
  new EntanglingIPrefetcherConfig
)

