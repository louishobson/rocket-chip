class CumulativeAdder(bitWidth: Int) extends Module {

    assert(bitWidth > 4, "Bit width of CumulativeAdder internal state must be greater than 4")

    val io = IO(new Bundle {
        val in = Flipped(Valid(UInt(4.W)))
        val out = Output(UInt(bitWidth.W))
    })

    private val internalState = RegInit(0.U(bitWidth.W))

    when(io.in.valid) {
        internalState := internalState + io.in.bits
    }

    out := internalState
}