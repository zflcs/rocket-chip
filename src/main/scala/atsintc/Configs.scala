package freechips.rocketchip.atsintc

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.tile._
import freechips.rocketchip.devices.tilelink._

class WithCustomBootROM(resetAddress: BigInt, bootImgPath: String)
  extends Config((_, _, up) => { case BootROMLocated(x) =>
    up(BootROMLocated(x)).map(_.copy(hang = resetAddress, contentFileName = bootImgPath))
  })


class AtsintcConfig extends Config(
  new WithNBigCores(4) ++
    new WithNExtTopInterrupts(6) ++
    new WithTimebase((BigInt(10000000))) ++ // 10 MHz
    new WithDTS("freechips.rocketchip-unknown", Nil) ++
    new WithCustomBootROM(0x10000, "../common/boot/bootrom/bootrom.img") ++
    new WithDefaultMemPort ++
    new WithDefaultMMIOPort ++
    new WithDefaultSlavePort ++
    new WithoutTLMonitors ++
    new WithCoherentBusTopology ++
    new BaseSubsystemConfig
)

class AtsintcTestConfig extends Config(new DefaultConfig)