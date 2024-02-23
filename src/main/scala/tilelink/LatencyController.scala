// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util.{DecoupledIO, log2Up}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.LatencyPipe
import org.chipsalliance.cde.config.Parameters



/** [[TLLatencies]] latency parameters for different message types */
case class TLLatencies(
  putLatency: Int,
  getLatency: Int,
  atomicLatency: Int,
  hintLatency: Int,
  transferLatency: Int,
  asQueue: Boolean = false
)
object TLLatencies {
  /** Create latency parameters for a blocking controller */
  def block(latency: Int) = TLLatencies(putLatency = latency, getLatency = latency, atomicLatency = latency, hintLatency = latency, transferLatency = latency, asQueue = false)

  /** Create latency parameters for a queuing controller */
  def queue(latency: Int) = TLLatencies(putLatency = latency, getLatency = latency, atomicLatency = latency, hintLatency = latency, transferLatency = latency, asQueue = true)
}



/** [[TLLatencyController]] a TL adapter node in order to enforce minimum latencies on different message types */
class TLLatencyController(q: TLLatencies)(implicit p: Parameters) extends LazyModule
{
  /* Create a TileLink adapter node (this node) */
  val node = TLAdapterNode()

  /* Instantiate the lazy module implementation */
  lazy val module = new Impl

  /* Get the maximum latency */
  val maxLatency = q.putLatency max q.getLatency max q.atomicLatency max q.hintLatency max q.transferLatency

  /* Define the lazy module implementation */
  class Impl extends LazyModuleImp(this) {

    /* Iterate over the in/out nodes/edges together */
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>

      /* Get the number of source IDs that can be used (and need to be kept track of) */
      val endSourceId = edgeOut.client.endSourceId

      /* Initially fully connect up in and out.
       * The ready wire on the response channel will be updated below.
       */
      out :<>= in

      /* Branch depending on the implementation */
      if(q.asQueue) {

        /* Assert all latencies are equal */
        assert(q.putLatency == q.getLatency && q.getLatency == q.atomicLatency && q.atomicLatency == q.hintLatency && q.hintLatency == q.transferLatency)

        /* Add a queue on the D channel */
        in.d :<>= LatencyPipe(out.d, maxLatency)

      } else {

        /* Define registers to store timestamps requests for the A-channel.
        * These are decremented on each cycle. */
        val a_channel_ts = RegInit(VecInit(Seq.fill(endSourceId)(0.U(log2Up(maxLatency+1).W))))
        a_channel_ts.foreach(ts => when(ts =/= 0.U) { ts := ts - 1.U })

        /* Define a feed function for A channel requests and D channel responses. Given:
        * - a request opcode,
        * - a response opcode, and
        * - a minimum latency,
        * it record when the request was made, and will delay the response
        * until the minimum latency number of clock cycles have passed. 
        */
        def feedA(reqOpcode: UInt, respOpcode: UInt, latency: Int): Unit = {
          /* Reset the counter on each incoming message */
          when(in.a.valid && out.a.ready && in.a.bits.opcode === reqOpcode) {
            a_channel_ts(in.a.bits.source) := latency.U
          }
          /* Unready the master when the minimum latency has not passed yet */
          when(out.d.valid && out.d.bits.opcode === respOpcode && a_channel_ts(out.d.bits.source) =/= 0.U) {
            in.d.valid := false.B
            out.d.ready := false.B
          }
        }

        /* Feed the A channel */
        if(edgeOut.slave.anySupportGet) 
          feedA(TLMessages.Get, TLMessages.AccessAckData, q.getLatency)
        if(edgeOut.slave.anySupportPutFull) 
          feedA(TLMessages.PutFullData, TLMessages.AccessAck, q.putLatency)
        if(edgeOut.slave.anySupportPutPartial) 
          feedA(TLMessages.PutPartialData, TLMessages.AccessAck, q.putLatency)
        if(edgeOut.slave.anySupportArithmetic) 
          feedA(TLMessages.ArithmeticData, TLMessages.AccessAckData, q.getLatency)
        if(edgeOut.slave.anySupportLogical) 
          feedA(TLMessages.LogicalData, TLMessages.AccessAckData, q.getLatency)
        if(edgeOut.slave.anySupportHint) 
          feedA(TLMessages.Hint, TLMessages.HintAck, q.getLatency)
      }
    }    
  }
}



/** [[TLLatencyController]] constructs a LatencyController */
object TLLatencyController {
  /** Create a TLLatencyController with constant latency for all options */
  def apply(q: Int)(implicit p: Parameters): TLNode = apply(TLLatencies(
      putLatency = q, getLatency = q, atomicLatency = q, hintLatency = q, transferLatency = q))

  /** Maybe create a TLLatencyController */
  def apply(q: Option[TLLatencies])(implicit p: Parameters): TLNode =
    q.map(apply(_)(p)).getOrElse(TLTempNode())

  /** Create a TLLatencyController with user-specified parameters */
  def apply(q: TLLatencies)(implicit p: Parameters): TLNode =
  {
    val controller = LazyModule(new TLLatencyController(q))
    controller.node
  }
}
