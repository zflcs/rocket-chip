package freechips.rocketchip.atsintc

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._


object ATSINTCConsts {
  def base: BigInt = 0x10000000
  def size = 0x1000000
  def maxProcess = 4092
  def eihSize = 0x2128
  def maxEih = 0x10
  def eihControlSize = 0x80


  def psOffset(index: Int) = index * 0x1000   // process i base address offset
  def ipcOffset(index: Int) = psOffset(index) + 0x800   // process i IPC base address offset
  def eihOffset = psOffset(maxProcess + 1)
  def eihInnerOffset(index: Int) = eihOffset + eihControlSize + index * 0x8
}

case class ATSINTCParams(baseAddress: BigInt = ATSINTCConsts.base, intStages: Int = 0) {
  def address = AddressSet(baseAddress, ATSINTCConsts.size - 1)
}

case object ATSINTCKey extends Field[Option[ATSINTCParams]](None)

case class ATSINTCAttachParams(slaveWhere: TLBusWrapperLocation = CBUS)

case object ATSINTCAttachKey extends Field(ATSINTCAttachParams())

/** Asynchorous-Task-Scheduler-Interrupt Controller */
class ATSINTC(params: ATSINTCParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule {

  import ATSINTCConsts._

  val device = new SimpleDevice("ats-intc", Seq("riscv,ats-intc0")) {
    override val alwaysExtended: Boolean = true

    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      val extra = Map("interrupt-controller" -> Nil, "#interrupt-cells" -> Seq(ResourceInt(1)))
      Description(name, mapping ++ extra)
    }
  }

  val node: TLRegisterNode = TLRegisterNode(
    address = Seq(params.address),
    device = device,
    beatBytes = beatBytes,
    concurrency = 1) // limiting concurrency handles RAW hazards on claim registers

  val intnode: IntNexusNode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
    sinkFn = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false,
    inputRequiresOutput = false)

  /* Negotiated sizes */
  def nHarts = intnode.edges.out.map(_.source.num).sum

  lazy val module = new LazyModuleImp(this) {
    Annotated.params(this, params)

    // // The MMIO read write operations of External Interrupt Handler
    // val eihRegFields = Seq.tabulate(maxEih) { i => 
    //   // eihInnerOffset(i) -> Seq(
    //   //   RegField(64,
    //   //     RegReadFn { ready => (Bool(true), uirs(i).asUInt(63, 0)) },
    //   //     RegWriteFn { (valid, data) =>
    //   //       when(valid) {
    //   //         uirs(i).hartid := data(31, 16)
    //   //         uirs(i).mode := data(1)
    //   //         uirs(i).active := data(0)
    //   //       }
    //   //       Bool(true)
    //   //     },
    //   //     Some(RegFieldDesc(
    //   //       name = s"uirs_low_bits_$i",
    //   //       desc = s"User interrupt basic status (active, mode and target hartid) of receiver $i"))),
    //   // )

    // }

    // val psRegFields = Seq.tabulate(maxProcess) { i => 
    //   // psOffset(i) -> Seq()

    // }

    // val ipcRegFields = Seq.tabulate(maxProcess) { i => 
    //   // ipcOffset(i) -> Seq()

    // }

    // node.regmap((eihRegFields ++ psRegFields ++ ipcRegFields): _*)

    
    val testReg = Seq(0x00 -> Seq(
      RegField(64,
        RegReadFn { UInt(0x19990109) },
        (),
        Some(RegFieldDesc(
          name = s"test",
          desc = s"ATSINTC read"))),
    ))

    node.regmap((testReg): _*)
    
  }
}

/** Trait that will connect a ATSINTC to a subsystem */
trait CanHavePeripheryATSINTC {
  this: BaseSubsystem =>
  val atsintcOpt = p(ATSINTCKey).map { params =>
    val tlbus = locateTLBusWrapper(p(ATSINTCAttachKey).slaveWhere)
    val atsintc = LazyModule(new ATSINTC(params, cbus.beatBytes))
    atsintc.node := tlbus.coupleTo("atsintc") { TLFragmenter(tlbus) := _ }

    InModuleBody {
      atsintc.module.clock := tlbus.module.clock
      atsintc.module.reset := tlbus.module.reset
    }

    atsintc
  }
}