// See LICENSE.Hobson for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._



/** [[QueryableHistoryBuffer]] contains an implementation of a history buffer that can be queried.
 * 
 * Entries contain a valid bit which is invalidated on querying.
 * Any evicted, valid entry is also reported via the IO.
 */
class QueryableHistoryBuffer[T <: Data](typ: T, length: Int) extends Module {

    /* Define the IO */
    val io = IO(new Bundle {
        val insert = Flipped(Valid(typ))
        val query = Flipped(Valid(typ))
        val query_result = Output(Bool())
        val evicted = Valid(typ)
    })

    /* Define The queue and its head pointer */
    assert(length >= 1, "HistoryBufferForTesting: queue length must be at least 1")
    val queue = RegInit(VecInit(Seq.fill(length)(Valid(typ).Lit(_.valid -> false.B))))
    val head = RegInit((length-1).U(log2Up(length).W))
  
    /* Define the insert operation */
    when(io.insert.valid) {
        /* Insert into the queue */
        queue(head) := io.insert

        /* Decrement the head pointer */
        when(head === 0.U) {
            head := (length-1).U
        } .otherwise {
            head := head - 1.U
        }

        /* Report on valid, evicted entries */
        io.evicted := queue(head)
    } .otherwise {
        /* Otherwise we need to say that nothing was evicted */
        io.evicted.valid := false.B
        io.evicted.bits := DontCare
    }

    /* Define the query operation */
    io.query_result := false.B
    when(io.query.valid) {
        /* Do a parallel lookup (last assignment to io.result wins) */
        queue.foreach(e => 
        {
            when(e.valid && e.bits === io.query.bits) {
                e.valid := false.B
                io.query_result := true.B
            }
        })
    }
}
