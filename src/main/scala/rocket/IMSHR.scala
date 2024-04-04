package freechips.rocketchip.rocket

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util.{Decoupled, Queue, RegEnable, log2Up}
import freechips.rocketchip.amba.AMBAProt
import freechips.rocketchip.tile.{CoreBundle, CoreModule}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{SeqToAugmentedSeq, ExposedShiftQueue, Split}
import org.chipsalliance.cde.config.Parameters

class IMSHRReq(implicit p: Parameters) extends CoreBundle with HasL1ICacheParameters {
  val index = UInt(idxBits.W)
  val paddr = UInt(paddrBits.W)
  val cache = Bool()
}

class IMSHRResp(ep: TLEdgeParameters)(implicit p: Parameters) extends CoreBundle with HasL1ICacheParameters {
  val index = UInt(idxBits.W)
  val paddr = UInt(paddrBits.W)
  val count = UInt(log2Up(ep.maxTransfer / ep.manager.beatBytes).W)
  val beat = Bool()
  val done = Bool()
  val demand = Bool()
  val denied = Bool()
  val corrupt = Bool()
  val data = UInt(ep.bundle.dataBits.W)
}

class IMSHR(edge: TLEdgeOut)(implicit p: Parameters) extends CoreModule with HasL1ICacheParameters {

  /* Define the IO */
  val io = IO(new Bundle {
    val demand_req = Flipped(Decoupled(new IMSHRReq))
    val prefetch_req = Flipped(Decoupled(new IMSHRReq))
    val a_channel = Decoupled(new TLBundleA(edge.bundle))
    val d_channel = Flipped(Decoupled(new TLBundleD(edge.bundle)))
    val resp = Decoupled(new IMSHRResp(edge))
    val sending_hint = Output(Bool())
    val hint_outstanding = Output(Bool())
    val soft_prefetch = Input(Bool())
  })

  /* Define the status registers */
  val status = RegInit(VecInit(Seq.fill(nPrefetchMSHRs+1)((new Bundle {
    val index = UInt(idxBits.W)
    val paddr = UInt(paddrBits.W)
    val valid = Bool()
    val demand = Bool()
  }).Lit(_.valid -> false.B))))

  /* Create filters to always consume requests which are already in the MSHR */
  val demand_filt   = Wire(Decoupled(new IMSHRReq))
  val prefetch_filt = Wire(Decoupled(new IMSHRReq))

  /* Create input queues in order to cache memory requests and service them as fast as possible */
  val (demand_q,   demand_q_elts  ) = ExposedShiftQueue(demand_filt,   entries=1, pipe=true, flow=true)
  val (prefetch_q, prefetch_q_elts) = ExposedShiftQueue(prefetch_filt, entries=4, pipe=true, flow=true)

  /* Detect whether an incomming demand or prefetch request corresponds to an already-inflight request.
   * This is a request with a matching physical address that is either already in the MSHR, or queued.
   */
  val demand_inflight = status.map(s => s.valid && s.paddr === io.demand_req.bits.paddr).asUInt.orR || 
    demand_q_elts.map  (r => r.valid && r.bits.paddr === io.demand_req.bits.paddr).asUInt.orR || 
    prefetch_q_elts.map(r => r.valid && r.bits.paddr === io.demand_req.bits.paddr).asUInt.orR
  val prefetch_inflight = status.map(s => s.valid && s.paddr === io.prefetch_req.bits.paddr).asUInt.orR || 
    demand_q_elts.map  (r => r.valid && r.bits.paddr === io.prefetch_req.bits.paddr).asUInt.orR || 
    prefetch_q_elts.map(r => r.valid && r.bits.paddr === io.prefetch_req.bits.paddr).asUInt.orR ||
    (io.demand_req.valid && io.demand_req.bits.paddr === io.prefetch_req.bits.paddr)

  /* Always accept but immediately discard duplicate requests */
  demand_filt.valid   := io.demand_req.valid   && !demand_inflight
  prefetch_filt.valid := io.prefetch_req.valid && !prefetch_inflight
  demand_filt.bits   := io.demand_req.bits
  prefetch_filt.bits := io.prefetch_req.bits  
  io.demand_req.ready   := demand_filt.ready   || demand_inflight
  io.prefetch_req.ready := prefetch_filt.ready || prefetch_inflight

  /* We are ready for a response when it either had no data (meaning it is a hint response), or the I$ is ready */
  io.d_channel.ready := !edge.hasData(io.d_channel.bits) || io.resp.ready

  /* Whether we could accept another request (ignoring whether the TL channel is ready) */
  val can_accept_demand_req = demand_q.valid && !status.map(_.valid).asUInt.andR
  val can_accept_prefetch_req = hasPrefetcher.B && prefetch_q.valid && !status.tail.map(_.valid).asUInt.andR

  /* We will accept the next demand request whenever we have space and the A TL port is ready */
  demand_q.ready := can_accept_demand_req && io.a_channel.ready

  /* We will accept the next prefetch request when
   *  - The prefetcher is enabled,
   *  - there is a valid request (prefetch_q.valid is asserted),
   *  - we have space,
   *  - the A TL port is ready, and
   *  - a demand request isn't currently being accepted.
   */
 prefetch_q.ready := can_accept_prefetch_req && !can_accept_demand_req && io.a_channel.ready

  /* Get the next request */
  val req = Mux(can_accept_demand_req, demand_q.bits, prefetch_q.bits)
  assert(req.paddr(blockOffBits-1,0) === 0.U)

  /* Default the channel */
  io.a_channel.valid := io.sending_hint || can_accept_demand_req || can_accept_prefetch_req
  io.a_channel.bits := DontCare

  /* Set the bits of the A channel */
  when(io.a_channel.valid && !io.sending_hint) {

    /* Iterate over the possibility of inserting into each register */
    for(i <- 0 to nPrefetchMSHRs) {
      val mask = (0 to nPrefetchMSHRs).map(_ < i).map(_.B).asUInt
      val free = status.tail.map(!_.valid).prepended(!status(0).valid && can_accept_demand_req).asUInt
      when(free(i) && (mask & free) === 0.U) {

        /* Make the request to higher-level memory */
        io.a_channel.bits := createGetRequest(req.paddr, req.cache, i)

        /* Save the request */
        when(io.a_channel.ready) {
          status(i).index := req.index
          status(i).paddr := req.paddr
          status(i).valid := true.B
          status(i).demand := can_accept_demand_req
        }
      }
    }
  }

  /* Remember whether we have an outstanding hint */
  val hint_outstanding = RegInit(false.B)
  io.hint_outstanding := hint_outstanding
  io.sending_hint := false.B

  /* Send hints when the prefetcher is not enabled */
  if (cacheParams.prefetch && !hasPrefetcher) {
    /* Save the previous request and whether it was actually sent */
    val prev_req = RegNext(req)
    val a_fired = RegNext(io.a_channel.fire)

    /* Send a prefetch request on the next cycle */
    when(a_fired && io.soft_prefetch && !hint_outstanding) {
      /* We should never be able to accept another request here */
      assert(!can_accept_demand_req)

      /** [[crosses_page]]  indicate if there is a crosses page access
        * [[next_block]] : the address to be prefetched.
        */
      val (crosses_page, next_block) = Split(prev_req.paddr(pgIdxBits-1, blockOffBits) +& 1.U, pgIdxBits-blockOffBits)

      /* Send the hint */
      io.sending_hint := crosses_page === next_block
      hint_outstanding := io.sending_hint
      io.a_channel.bits := createHintRequest(req.paddr, next_block)
    }

    /* Unset hint_outstanding when we get the ACK */
    when(io.d_channel.valid && !edge.hasData(io.d_channel.bits)) {
      hint_outstanding := false.B
    }
  }

  /* Collect D-channel response info I$ */
  val response_valid = io.d_channel.valid && edge.hasData(io.d_channel.bits)
  val response_fire = response_valid && io.resp.ready
  val (_, _, response_done, response_count) = edge.count(io.d_channel)
  val response_status = status(io.d_channel.bits.source)
  assert(!response_valid || response_status.valid)

  /* Save the response into a wire */
  val response = Wire(new IMSHRResp(edge))
  response.done := response_done
  response.count := response_count
  response.beat := DontCare
  response.index := response_status.index
  response.paddr := response_status.paddr
  response.demand := response_status.demand
  response.denied := io.d_channel.bits.denied
  response.corrupt := io.d_channel.bits.corrupt
  response.data := io.d_channel.bits.data

  /* Keep track of ongoing bursts */
  val ongoing_burst = RegInit(false.B)
  when(response_fire && response_done) {
    ongoing_burst := false.B
    response_status.valid := false.B
  } .elsewhen (response_valid) {
    ongoing_burst := true.B
  }

  /* Keep saving valid responses into a register.
   * This way we can still tell the I$ about an ongoing burst in between beats. 
   */
  val response_reg = RegEnable(response, response_fire)

  /* Actually send the response to the I$ */
  io.resp.valid := response_valid || ongoing_burst
  io.resp.bits := Mux(response_valid, response, response_reg)
  io.resp.bits.beat := response_valid

  /* Get information about the prefetcher */
  def entanglingParams = cacheParams.entanglingParams
  def hasPrefetcher = entanglingParams.isDefined
  def nPrefetchMSHRs = entanglingParams.map(_.nPrefetchMSHRs).getOrElse(0)

  /* Make a Access request to the L2 memory */
  def createGetRequest(paddr: UInt, cache: Bool, source: Int): TLBundleA = {
    val bits = Wire(new TLBundleA(edge.bundle)) 
    bits := edge.Get(
      fromSource = source.U, 
      toAddress = paddr,
      lgSize = lgCacheBlockBytes.U
    )._2
    bits.user.lift(AMBAProt).foreach { x =>
      /* Rocket caches all fetch requests, and it's difficult to differentiate privileged/unprivileged on
       * cached data, so mark as privileged.
       */
      x.fetch       := true.B
      x.secure      := true.B
      x.privileged  := true.B
      x.bufferable  := true.B
      x.modifiable  := true.B
      x.readalloc   := cache
      x.writealloc  := cache
    }
    bits
  }

  /* Make a hint request to the L2 memory */
  def createHintRequest(paddr: UInt, next_block: UInt) = edge.Hint(
    fromSource = 1.U,
    toAddress = ((paddr >> pgIdxBits) ## next_block) << blockOffBits,
    lgSize = lgCacheBlockBytes.U,
    param = TLHints.PREFETCH_READ
  )._2

}
