package core

import chisel3._
import chisel3.util._

import common.Consts._
import _root_.circt.stage.ChiselStage

class Top(memoryFile: String, suppressLog: Boolean) extends Module {
  val io = IO(new Bundle {
    val gp = Output(UInt(WORD_LEN.W))
    val pc = Output(UInt(WORD_LEN.W))
    val inst = Output(UInt(WORD_LEN.W))
  })

  val regfile = Module(new RegFile())
  val csrfile = Module(new CsrFile())
  val core = Module(new Core(suppressLog))
  val bus = Module(new Bus())
  val memory = Module(new Memory(memoryFile))
  val uart_memory = Module(new UartMemory())
  val clint_memory = Module(new ClintMemory())

  core.io.imem <> memory.io.imem
  core.io.dmem <> bus.io.dmem_in
  core.io.regfile <> regfile.io
  core.io.csrfile_regs_r <> csrfile.io.regs_r
  core.io.csrfile_command <> csrfile.io.command
  core.io.csrfile_trap <> csrfile.io.trap
  bus.io.dmem_out_memory <> memory.io.dmem
  bus.io.dmem_out_uart <> uart_memory.io.dmem
  bus.io.dmem_out_clint <> clint_memory.io.dmem
  clint_memory.io.csrfile_regs_rw <> csrfile.io.regs_rw
  io.gp := core.io.gp
  io.pc := core.io.pc
  io.inst := core.io.inst
}

object Elaborate extends App {
  ChiselStage.emitSystemVerilogFile(
    new Top("src/hex/coremark.hex", false),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}
