package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import common.Consts._

class ImemPortIo extends Bundle {
  val addr = Input(UInt(WORD_LEN.W))
  val inst = Output(UInt(WORD_LEN.W))
}

object MenSel extends ChiselEnum {
  val X, B, H, W = Value
}

class DmemPortIo extends Bundle {
  val raddr = Input(UInt(WORD_LEN.W))
  val rdata = Output(UInt(WORD_LEN.W))
  val wen = Input(MenSel())
  val waddr = Input(UInt(WORD_LEN.W))
  val wdata = Input(UInt(WORD_LEN.W))
}

class Memory(memoryFile: String) extends Module {
  val io = IO(new Bundle {
    val imem = new ImemPortIo()
    val dmem = new DmemPortIo()
  })

  val mem = Mem(0x10000, UInt(8.W))

  if (memoryFile.nonEmpty) {
    loadMemoryFromFileInline(mem, memoryFile)
  }

  io.imem.inst := Cat(
    mem(io.imem.addr + 3.U(WORD_LEN.W)),
    mem(io.imem.addr + 2.U(WORD_LEN.W)),
    mem(io.imem.addr + 1.U(WORD_LEN.W)),
    mem(io.imem.addr + 0.U(WORD_LEN.W))
  )

  io.dmem.rdata := Cat(
    mem(io.dmem.raddr + 3.U(WORD_LEN.W)),
    mem(io.dmem.raddr + 2.U(WORD_LEN.W)),
    mem(io.dmem.raddr + 1.U(WORD_LEN.W)),
    mem(io.dmem.raddr + 0.U(WORD_LEN.W))
  )

  when(io.dmem.wen === MenSel.B) {
    mem(io.dmem.waddr + 0.U(WORD_LEN.W)) := io.dmem.wdata(7, 0)
  }.elsewhen(io.dmem.wen === MenSel.H) {
    mem(io.dmem.waddr + 0.U(WORD_LEN.W)) := io.dmem.wdata(7, 0)
    mem(io.dmem.waddr + 1.U(WORD_LEN.W)) := io.dmem.wdata(15, 8)
  }.elsewhen(io.dmem.wen === MenSel.W) {
    mem(io.dmem.waddr + 0.U(WORD_LEN.W)) := io.dmem.wdata(7, 0)
    mem(io.dmem.waddr + 1.U(WORD_LEN.W)) := io.dmem.wdata(15, 8)
    mem(io.dmem.waddr + 2.U(WORD_LEN.W)) := io.dmem.wdata(23, 16)
    mem(io.dmem.waddr + 3.U(WORD_LEN.W)) := io.dmem.wdata(31, 24)
  }
}
