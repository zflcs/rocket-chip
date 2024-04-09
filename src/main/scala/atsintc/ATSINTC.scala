package freechips.rocketchip.atsintc

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.interrupts._


object ATSINTCConsts {
  def base: BigInt = 0x10000000
  def size = 0x1000000
  def maxProcess = 4092
  def eihSize = 0x2128
  def maxEih = 0x10
  def eihControlSize = 0x80
  def numPrio = 8
  def dataWidth = 64
  def capacity = 1024
  def bq_capacity = 4


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

  val intnode: IntNexusNode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false,
    inputRequiresOutput = false)

    def nDevices: Int = intnode.edges.in.map(_.source.num).sum

  lazy val module = new LazyModuleImp(this) {
    Annotated.params(this, params)

    // Compact the interrupt vector the same way
    val interrupts = intnode.in.map { case (i, e) => i.take(e.source.num) }.flatten
    
    println(s"ATSINTC map ${nDevices} external interrupts:")

    val queue = Module(new PQWithExtIntrHandler(nDevices, ATSINTCConsts.bq_capacity, ATSINTCConsts.numPrio, ATSINTCConsts.capacity, ATSINTCConsts.dataWidth))
    for(i <- 0 until nDevices) {
      queue.io.intrs(i) := interrupts(i)
    }

    val deqReg = Seq(0x00 -> Seq(RegField.r(ATSINTCConsts.dataWidth, queue.io.deq)))
    val enqRegs = Seq.tabulate(ATSINTCConsts.numPrio) { i =>
      0x08 + 8 * i -> Seq(RegField.w(ATSINTCConsts.dataWidth, queue.io.enqs(i)))
    }
    val simExtIntrRegs = Seq.tabulate(nDevices) { i => 
      0x200000 + 8 * i -> Seq(RegField.w(ATSINTCConsts.dataWidth, RegWriteFn { (valid, data) =>
        val tmp = RegNext(valid)
        queue.io.intrs(i) := valid || tmp
        true.B
      }))
    }
    val extintrRegs = Seq.tabulate(nDevices) { i =>
      0x900 + 8 * i -> Seq(RegField.w(ATSINTCConsts.dataWidth, queue.io.intrh_enqs(i)))
    }

    node.regmap((deqReg ++ enqRegs ++ extintrRegs ++ simExtIntrRegs): _*)
    
  }
}

/** Trait that will connect a ATSINTC to a subsystem */
trait CanHavePeripheryATSINTC {
  this: BaseSubsystem =>
  val atsintcOpt = p(ATSINTCKey).map { params =>
    val tlbus = locateTLBusWrapper(p(ATSINTCAttachKey).slaveWhere)
    val atsintc = LazyModule(new ATSINTC(params, cbus.beatBytes))
    atsintc.node := tlbus.coupleTo("atsintc") { TLFragmenter(tlbus) := _ }
    atsintc.intnode :=* ibus.toPLIC

    InModuleBody {
      atsintc.module.clock := tlbus.module.clock
      atsintc.module.reset := tlbus.module.reset
    }

    atsintc
  }
}