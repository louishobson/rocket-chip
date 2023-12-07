// See LICENSE.SiFive for license details.

package freechips.rocketchip.tiletest

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.AsyncResetReg
import freechips.rocketchip.system._
import freechips.rocketchip.unittest._

class TestHarness()(implicit p: Parameters) extends Module {
  /* Define the IO */
  val io = IO(new Bundle {
    val success = Output(Bool())
  })

  /* Instantiate the LazyModule for the Rocket system, but DO NOT make reference to the inner module.
   * This way Diplomacy will negotiate all of the parameters, but won't actually instantiate tiles.
   */
  val lRocketSystem = LazyModule(new ExampleRocketSystem)

  /* Now we want to run a test suite for each core in the system */
  io.success := lRocketSystem.tiles.map(tile => Module(new UnitTestSuite()(tile.p)).io.finished).reduce(_&&_)

}
