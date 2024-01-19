package freechips.rocketchip.atsintc

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.util._

/** Top module for zynq fpga, which will be wrapped with SoC block design  */
class Top()(implicit p: Parameters) extends Module {
  val target = Module(LazyModule(new FPGAZynqTop).module)

  require(target.mem_axi4.size == 1)
  require(target.mmio_axi4.size == 1)
  require(target.l2_frontend_bus_axi4.size == 1)

  val io = IO(new Bundle {
    val mem_axi4 = target.mem_axi4.head.cloneType
    val mmio_axi4 = target.mmio_axi4.head.cloneType
    val l2_frontend_bus_axi4 = Flipped(target.l2_frontend_bus_axi4.head.cloneType)
    val sys_reset = Input(Bool())
    val ext_intrs = Input(UInt(p(NExtTopInterrupts).W))
  })

  io.mem_axi4 <> target.mem_axi4.head
  io.mmio_axi4 <> target.mmio_axi4.head
  target.l2_frontend_bus_axi4.head <> io.l2_frontend_bus_axi4

  target.reset := reset | io.sys_reset // outer reset
  target.interrupts := io.ext_intrs

  target.dontTouchPorts()
}

class TestHarness()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
  })

  val ldut = LazyModule(new FPGAZynqTop)
  val dut = Module(ldut.module)

  // Allow the debug ndreset to reset the dut, but not until the initial reset has completed
  dut.reset := (reset.asBool | dut.debug.map { debug => AsyncResetReg(debug.ndreset) }.getOrElse(false.B)).asBool

  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  SimAXIMem.connectMem(ldut)
  SimAXIMem.connectMMIO(ldut)
  Debug.connectDebug(dut.debug, dut.resetctrl, dut.psd, clock, reset.asBool, io.success)
}

class FPGAZynqTop(implicit p: Parameters) extends RocketSubsystem
  with HasAsyncExtInterrupts
  with CanHaveSlaveAXI4Port
  with CanHaveMasterAXI4MemPort
  with CanHaveMasterAXI4MMIOPort {
  // Note that setting BootROMLocated will override the reset_vector for all tiles
  val bootROM = p(BootROMLocated(location)).map { params =>
    println(s"Init BootROM ${params.hang} ${params.contentFileName}")
    BootROM.attach(params, this, CBUS)}

  override lazy val module = new FPGAZynqTopModuleImp(this)
}

class FPGAZynqTopModuleImp(_outer: FPGAZynqTop) extends RocketSubsystemModuleImp(_outer)
  with HasRTCModuleImp
  with HasExtInterruptsModuleImp
  with DontTouch {
  lazy val mem_axi4 = outer.mem_axi4
  lazy val mmio_axi4 = outer.mmio_axi4
  lazy val l2_frontend_bus_axi4 = outer.l2_frontend_bus_axi4
}