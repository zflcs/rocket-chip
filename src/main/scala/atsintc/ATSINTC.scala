package freechips.rocketchip.atsintc

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
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

  lazy val module = new LazyModuleImp(this) {
    Annotated.params(this, params)

    val queue = Module(new Queue(UInt(64.W), 4))
    val testReg = Seq(
      0x00 -> Seq(RegField.r(64, queue.io.deq)),
      0x08 -> Seq(RegField.w(64, queue.io.enq))
    )
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