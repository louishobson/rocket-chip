package freechips.rocketchip.tiletest

import chisel3._
import freechips.rocketchip.rocket._
import freechips.rocketchip.system._
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._
import org.chipsalliance.cde.config._

class WithEntanglingTests extends Config((site, here, up) => {
  case UnitTests => (q: Parameters) => {
    implicit val p = q
    Seq(
      /* TEST 1: We can encode any two arbitrary addresses without compression */
      Module(new EntanglingTest(0, 0x800000, Seq(0, 0xFFFFFF), 0)),

      /* TEST 2: We can encode six highly-compressed addresses */
      Module(new EntanglingTest(1, 0x800000, Seq.tabulate(6)(0x800000+_), 0)),

      /* TEST 3: We should drop one address from a sequence of 7, even when they are compressible */
      Module(new EntanglingTest(2, 0x800000, Seq.tabulate(7)(0x800000+_), 1)),

      /* TEST 4: We should drop down to two address if none are compressible */
      Module(new EntanglingTest(3, 0x800000, (0 until 7), 5)),

      /* TEST 5: We can encode the empty sequence of baddrs */
      Module(new EntanglingTest(4, 0x800000, Seq(), 0)),
    )
  }
})

class EntanglingIPrefetcherUnitTestConfig extends Config(new WithEntanglingTests ++ new EntanglingIPrefetcherConfig)

