package core

import chisel3._
import chisel3.util._

import common.Consts._
import _root_.circt.stage.ChiselStage

class Top(memoryFile: String, suppressLog: Boolean) extends Module {
  val io = IO(new Bundle {
    val exit = Output(Bool())
    val gp = Output(UInt(WORD_LEN.W))
  })

  val regfile = Module(new RegFile())
  val csrfile = Module(new CsrFile())
  val core = Module(new Core(suppressLog))
  val memory = Module(new Memory(memoryFile))
  core.io.imem <> memory.io.imem
  core.io.dmem <> memory.io.dmem
  core.io.regfile <> regfile.io
  core.io.csrfile <> csrfile.io
  io.exit := core.io.exit
  io.gp := core.io.gp
}

object Elaborate extends App {
  ChiselStage.emitSystemVerilogFile(
    new Top("src/hex/pcnt.hex", false),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}
