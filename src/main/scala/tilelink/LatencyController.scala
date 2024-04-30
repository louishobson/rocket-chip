// See LICENSE.SiFive for license details.
// See LICENSE.Hobson for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Queue, log2Up}
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters



/** [[TLLatencies]] latency parameters for different message types */
case class TLLatencies(
  latency: Int,
  asQueue: Boolean = false,
  queuePacketPeriod: Int = 1,
)
object TLLatencies {
  /** Create latency parameters for a blocking controller */
  def block(latency: Int) = TLLatencies(latency, asQueue = false)

  /** Create latency parameters for a queuing controller */
  def queue(latency: Int, queuePacketPeriod: Int = 1) = TLLatencies(latency, asQueue = true, queuePacketPeriod = queuePacketPeriod)
}



/** [[LatencyThroughputPipe]] Like [[LatencyPipe]], but it allows propagation of data at a slower rate. */
class LatencyThroughputPipe[T <: Data](typ: T, latency: Int, period: Int) extends Module {
  
  /* We need latency/period to be an integer */
  assert(latency % period == 0, "Latency must be a product of the period to create a LatencyThroughputPipe")
  
  /* The period should be positive */
  assert(period >= 1, "The period must be positive to create a LatencyThroughputPipe")

  /* Define the IO */
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Decoupled(typ)
  })

  /* Add a counter to reduce throughput */
  val queue_period_cnt = RegInit(0.U(log2Up(period).W))

  /* Reset the counter on on transfer, otherwise decrement it */
  when(io.in.fire) {
    queue_period_cnt := (period-1).U
  } .elsewhen(queue_period_cnt =/= 0.U) {
    queue_period_cnt := queue_period_cnt - 1.U
  }  

  /* Create a decoupled IO which is gated based on queue_period_cnt */
  def gated[T <: Data](in: DecoupledIO[T]): DecoupledIO[T] = {
    val out = Wire(chiselTypeOf(in))
    out.valid := in.valid && queue_period_cnt === 0.U
    in.ready := out.ready && queue_period_cnt === 0.U
    out.bits := in.bits
    out
  }

  /* Connect the output to lots of gated queues */
  io.out :<>= gated((0 until (latency/period)).foldLeft(io.in)((last_io, _) => Queue(gated(last_io), 1, true)))
}

/** [[LatencyThroughputPipe]] Constructs a LatencyThroughputPipe */ 
object LatencyThroughputPipe {
  def apply[T <: Data](in: DecoupledIO[T], latency: Int, period: Int): DecoupledIO[T] = {
    val pipe = Module(new LatencyThroughputPipe(chiselTypeOf(in.bits), latency, period))
    pipe.io.in :<>= in
    pipe.io.out
  }
}




/** [[TLLatencyController]] a TL adapter node in order to enforce minimum latencies on different message types */
class TLLatencyController(q: TLLatencies)(implicit p: Parameters) extends LazyModule {

  /* Assert that the latency is positive */
  assert(q.latency >= 0)

  /* Create a TileLink adapter node (this node) */
  val node = TLAdapterNode()

  /* Instantiate the lazy module implementation */
  lazy val module = new Impl

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

        /* Assert that the queue packet period is a factor of the latency */
        assert(q.queuePacketPeriod >= 1 && q.latency % q.queuePacketPeriod == 0)

        /* Create a Latency(Throughput)Pipe */
        in.d :<>= LatencyThroughputPipe(out.d, q.latency, q.queuePacketPeriod)  

      } else {

        /* Define registers to store timestamps requests for the A-channel.
        * These are decremented on each cycle. */
        val a_channel_ts = RegInit(VecInit(Seq.fill(endSourceId)(0.U(log2Up(q.latency+1).W))))
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
          feedA(TLMessages.Get, TLMessages.AccessAckData, q.latency)
        if(edgeOut.slave.anySupportPutFull) 
          feedA(TLMessages.PutFullData, TLMessages.AccessAck, q.latency)
        if(edgeOut.slave.anySupportPutPartial) 
          feedA(TLMessages.PutPartialData, TLMessages.AccessAck, q.latency)
        if(edgeOut.slave.anySupportArithmetic) 
          feedA(TLMessages.ArithmeticData, TLMessages.AccessAckData, q.latency)
        if(edgeOut.slave.anySupportLogical) 
          feedA(TLMessages.LogicalData, TLMessages.AccessAckData, q.latency)
        if(edgeOut.slave.anySupportHint) 
          feedA(TLMessages.Hint, TLMessages.HintAck, q.latency)
      }
    }    
  }
}



/** [[TLLatencyController]] constructs a LatencyController */
object TLLatencyController {
  /** Create a TLLatencyController with specific parameters */
  def apply(latency: Int, asQueue: Boolean = false, queuePacketPeriod: Int = 1)(implicit p: Parameters): TLNode = 
    apply(TLLatencies(latency, asQueue, queuePacketPeriod))

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
