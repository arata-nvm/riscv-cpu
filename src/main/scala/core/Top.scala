package core

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage;
import common.Consts._

class Top(memoryFile: String) extends Module {
  val io = IO(new Bundle {
    val exit = Output(Bool())
    val gp = Output(UInt(WORD_LEN.W))
  })

  val core = Module(new Core())
  val memory = Module(new Memory(memoryFile))
  core.io.imem <> memory.io.imem
  core.io.dmem <> memory.io.dmem
  io.exit := core.io.exit
  io.gp := core.io.gp
}

object Elaborate extends App {
  println(
    ChiselStage.emitSystemVerilog(
      gen = new Top("src/hex/pcnt.hex")
    )
  )
}
