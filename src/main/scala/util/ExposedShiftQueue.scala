// See LICENSE.Hobson for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

/** [[ExposedShiftQueue]] exposes the contents of a [[ShiftQueue]] as an extra IO port.
 */
class ExposedShiftQueue[T <: Data](
  gen: T,
  entries: Int,
  pipe: Boolean = false,
  flow: Boolean = false
) extends ShiftQueue(gen, entries, pipe, flow) {
  /* Add the extra IO port */
  val exposed_io = IO(new Bundle{
    val items = Output(Vec(entries, Valid(gen)))
  })

  /* Write the contents of the queue to the extra IO port */
  exposed_io.items.zip(elts).foreach{case (i, e) => i.bits := e} 
  exposed_io.items.zip(valid).foreach{case (i, v) => i.valid := v}
}

/** [[ExposedShiftQueue]] object constructs an [[ExposedShiftQueue]], 
 * returning its dequeue interface and exposed contents as a pair.
 */
object ExposedShiftQueue
{
  def apply[T <: Data](enq: DecoupledIO[T], entries: Int = 2, pipe: Boolean = false, flow: Boolean = false): (DecoupledIO[T], Vec[ValidIO[T]]) = {
    val q = Module(new ExposedShiftQueue(enq.bits.cloneType, entries, pipe, flow))
    q.io.enq <> enq
    (q.io.deq, q.exposed_io.items)
  }
}