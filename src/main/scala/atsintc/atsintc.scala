// package freechips.rocketchip.atsintc

// import Chisel._
// import freechips.rocketchip.config.{Field, Parameters}
// import freechips.rocketchip.diplomacy._
// import freechips.rocketchip.interrupts._
// import freechips.rocketchip.regmapper._
// import freechips.rocketchip.subsystem._
// import freechips.rocketchip.tilelink._
// import freechips.rocketchip.util._

// class UIRS extends Bundle {
//   val pending = UInt(64.W)
//   val zero1 = UInt(32.W)
//   val hartid = UInt(16.W)
//   val zero0 = UInt(14.W)
//   val mode = Bool()
//   val active = Bool()
// }

// object ATSINTCConsts {
//   def base: BigInt = 0x0F000000
//   def size = 0x1000000
//   def opBytes = 32

//   def sendOpOffset = 0x00
//   def lowOpOffset = 0x08
//   def highOpOffset = 0x10
//   def actOpOffset = 0x18

//   def opOffset(index: Int) = index << log2Ceil(opBytes)
//   def sendOffset(index: Int) = opOffset(index) + sendOpOffset
//   def lowOffset(index: Int) = opOffset(index) + lowOpOffset
//   def highOffset(index: Int) = opOffset(index) + highOpOffset
//   def actOffset(index: Int) = opOffset(index) + actOpOffset

//   def nRecvs = size / opBytes
// }

// case class UINTCParams(baseAddress: BigInt = UINTCConsts.base, intStages: Int = 0) {
//   def address = AddressSet(baseAddress, UINTCConsts.size - 1)
// }

// case object UINTCKey extends Field[Option[UINTCParams]](None)

// case class UINTCAttachParams(slaveWhere: TLBusWrapperLocation = CBUS)

// case object UINTCAttachKey extends Field(UINTCAttachParams())

// /** User-Interrupt Controller */
// class UINTC(params: UINTCParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule {

//   import UINTCConsts._

//   val device = new SimpleDevice("uintc", Seq("riscv,uintc0")) {
//     override val alwaysExtended: Boolean = true

//     override def describe(resources: ResourceBindings): Description = {
//       val Description(name, mapping) = super.describe(resources)
//       val extra = Map("interrupt-controller" -> Nil, "#interrupt-cells" -> Seq(ResourceInt(1)))
//       Description(name, mapping ++ extra)
//     }
//   }

//   val node: TLRegisterNode = TLRegisterNode(
//     address = Seq(params.address),
//     device = device,
//     beatBytes = beatBytes,
//     concurrency = 1) // limiting concurrency handles RAW hazards on claim registers

//   /* Only ouput */
//   val intnode: IntNexusNode = IntNexusNode(
//     sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
//     sinkFn = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
//     outputRequiresInput = false)

//   /* Negotiated sizes */
//   def nHarts = intnode.edges.out.map(_.source.num).sum

//   def hartId(id: UInt) = if (nHarts <= 1) 0.U else id(log2Ceil(nHarts) - 1, 0)

//   lazy val module = new LazyModuleImp(this) {
//     Annotated.params(this, params)
//     require(intnode.edges.in.size == 0, "UINTC only produces interrupts; it does not accept them")

//     val uirs = Seq.fill(nRecvs) { RegInit(new UIRS().fromBits(0.U)) }

//     val ipi = Seq.fill(nHarts) { RegInit(0.U) }
//     ipi.zipWithIndex.foreach { case (hart, i) =>
//       hart := uirs.map(x => x.pending =/= 0.U && x.active && hartId(x.hartid) === i.asUInt).reduce(_ || _)
//     }
//     val (intnode_out, _) = intnode.out.unzip
//     intnode_out.zipWithIndex.foreach { case (int, i) =>
//       int(0) := ShiftRegister(ipi(i)(0), params.intStages) // usip
//     }

//     /* UINTC operation fields */

//     val opRegFields = Seq.tabulate(nRecvs) { i =>
//       opOffset(i) -> Seq(
//         RegField(64, (),
//           RegWriteFn { (valid, data) =>
//             uirs(i).pending := uirs(i).pending | (valid << data(5, 0)).asUInt
//             Bool(true)
//           },
//           Some(RegFieldDesc(
//             name = s"send_uipi_$i",
//             desc = s"Send user software interrupt to receiver $i if bits set to 1."))),
//         RegField(64,
//           RegReadFn { ready => (Bool(true), uirs(i).asUInt(63, 0)) },
//           RegWriteFn { (valid, data) =>
//             when(valid) {
//               uirs(i).hartid := data(31, 16)
//               uirs(i).mode := data(1)
//               uirs(i).active := data(0)
//             }
//             Bool(true)
//           },
//           Some(RegFieldDesc(
//             name = s"uirs_low_bits_$i",
//             desc = s"User interrupt basic status (active, mode and target hartid) of receiver $i"))),
//         RegField(64,
//           RegReadFn { ready =>
//             when(ready) {
//               uirs(i).pending := 0.U
//             }
//             (Bool(true), uirs(i).pending)
//           },
//           RegWriteFn { (valid, data) =>
//             when(valid) {
//               uirs(i).pending := uirs(i).pending | data
//             }
//             Bool(true)
//           },
//           Some(RegFieldDesc(
//             name = s"uirs_high_bits_$i",
//             desc = s"One bit for each user interrupt vector. There is user-interrupt request for a vector if the corresponding bit is 1.",
//             wrType = Some(RegFieldWrType.ONE_TO_SET),
//             rdAction = Some(RegFieldRdAction.CLEAR)))),
//         RegField(64, (),
//           RegWriteFn { (valid, data) =>
//             when(valid) {
//               uirs(i).active := data(0)
//             }
//             Bool(true)
//           }, None)
//       )
//     }

//     node.regmap((opRegFields): _*)
//   }
// }

// /** Trait that will connect a UINTC to a subsystem */
// trait CanHavePeripheryUINTC {
//   this: BaseSubsystem =>
//   val uintcOpt = p(UINTCKey).map { params =>
//     val tlbus = locateTLBusWrapper(p(UINTCAttachKey).slaveWhere)
//     val uintc = LazyModule(new UINTC(params, cbus.beatBytes))
//     uintc.node := tlbus.coupleTo("uintc") { TLFragmenter(tlbus) := _ }

//     InModuleBody {
//       uintc.module.clock := tlbus.module.clock
//       uintc.module.reset := tlbus.module.reset
//     }

//     uintc
//   }
// }