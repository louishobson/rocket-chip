package freechips.rocketchip.tiletest

import chisel3._
import freechips.rocketchip.rocket._
import freechips.rocketchip.system._
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._
import org.chipsalliance.cde.config._
import chisel3.experimental.BundleLiterals._

class WithEntanglingTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    class Tests(implicit val p: Parameters) extends HasEntanglingIPrefetcherParameters {
      def maxBaddr = (Math.pow(2, baddrBits)-1).toInt
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

class WithBasicBlockTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    class Tests(implicit val p: Parameters) extends HasEntanglingIPrefetcherParameters {
      val offsetBaddr = 128
      val offsetTime = 100
      def req(baddr: Int, time: Int, valid: Boolean = true) = ((new BBCounterReq).Lit(_.baddr -> (baddr+offsetBaddr).U, _.time -> (time+offsetTime).U), valid.B)
      def resp(head: Int, time: Int, size: Int, done: Boolean) = (new BBCounterResp).Lit(_.head -> (head+offsetBaddr).U, _.time -> (time+offsetTime).U, _.size -> size.U, _.done -> done.B)
      def produce = Seq(
        /* TEST 1: a sequence of consecutive addresses */
        Module(new BasicBlockTest(0, 
          Seq.tabulate(16)(i => req(i, i)),
          Seq.tabulate(16)(i => resp(0, 0, i+1, false))
        )),

        /* TEST 2: a sequence of increasing or equal addresses */
        Module(new BasicBlockTest(1, 
          Seq.tabulate(16)(i => req((i/2).toInt, i)),
          Seq.tabulate(16)(i => resp(0, 0, (i/2).toInt+1, false))
        )),

        /* TEST 3: a sequence of addresses all in one BB, but non-increasing */
        Module(new BasicBlockTest(2, 
          Seq(0, 1, 2, 3, 0, 1, 2, 3, 1, 2, 3, 4).map(i => req(i, i)),
          Seq(1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5).map(i => resp(0, 0, i, false))
        )),

        /* TEST 4: a new BB is started when the maximum BB size is exceeded */
        Module(new BasicBlockTest(3, 
          Seq.tabulate(maxBBSize+1)(i => req(i, i)),
          Seq.tabulate(maxBBSize-1)(i => resp(0, 0, i+1, false))
            ++ Seq(resp(0, 0, maxBBSize, true), resp(maxBBSize, maxBBSize, 1, false))
        )),

        /* TEST 5: a jump forwards starts a new BB */
        Module(new BasicBlockTest(4, 
          Seq(req(0, 0), req(1, 1), req(2, 2), req(4, 3), req(5, 3)),
          Seq(resp(0, 0, 1, false), resp(0, 0, 2, false), resp(0, 0, 3, true), resp(4, 3, 1, false), resp(4, 3, 2, false)),
        )),

        /* TEST 6: a jump backwards starts a new BB */
        Module(new BasicBlockTest(5, 
          Seq(req(1, 0), req(2, 1), req(3, 2), req(0, 3), req(1, 4)),
          Seq(resp(1, 0, 1, false), resp(1, 0, 2, false), resp(1, 0, 3, true), resp(0, 3, 1, false), resp(0, 3, 2, false)),
        )),

        /* TEST 7: inputs marked as invalid don't cause a change in BB */
        Module(new BasicBlockTest(6, 
          Seq(req(1, 0), req(2, 1), req(3, 2), req(0, 3, false), req(4, 4), req(6, 5, false), req(5, 6)),
          Seq(resp(1, 0, 1, false), resp(1, 0, 2, false), resp(1, 0, 3, false), resp(1, 0, 3, false), resp(1, 0, 4, false), resp(1, 0, 4, false), resp(1, 0, 5, false)),
        )),
      )
    }
    new Tests()(q).produce ++ up.lift(UnitTests).map(_(q)).getOrElse(Seq())
  }
})

class WithHistoryBufferTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    class Tests(implicit val p: Parameters) extends HasEntanglingIPrefetcherParameters {
      def insertReq(head: Int, time: Int) = (new HBInsertReq).Lit(_.head -> head.U, _.time -> time.U)
      def searchReq(time: Int) = (new HBSearchReq).Lit(_.targ_time -> time.U)
      def searchResp(head: Int) = Some((new HBSearchResp).Lit(_.head -> head.U))
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

class EntanglingIPrefetcherUnitTestConfig extends Config(new WithHistoryBufferTests ++ new WithEntanglingTests ++ new WithBasicBlockTests ++ new EntanglingIPrefetcherConfig)

