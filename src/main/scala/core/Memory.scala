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
  val addr = Input(UInt(WORD_LEN.W))
  val rdatab = Output(UInt(WORD_LEN.W))
  val rdatabu = Output(UInt(WORD_LEN.W))
  val rdatah = Output(UInt(WORD_LEN.W))
  val rdatahu = Output(UInt(WORD_LEN.W))
  val rdataw = Output(UInt(WORD_LEN.W))
  val wen = Input(MenSel())
  val wdata = Input(UInt(WORD_LEN.W))
}

class Memory(memoryFile: String) extends Module {
  val io = IO(new Bundle {
    val imem = new ImemPortIo()
    val dmem = new DmemPortIo()
  })

  val mem = Mem(16384, UInt(8.W))

  if (memoryFile.nonEmpty) {
    loadMemoryFromFileInline(mem, memoryFile)
  }

  io.imem.inst := Cat(
    mem(io.imem.addr + 3.U(WORD_LEN.W)),
    mem(io.imem.addr + 2.U(WORD_LEN.W)),
    mem(io.imem.addr + 1.U(WORD_LEN.W)),
    mem(io.imem.addr + 0.U(WORD_LEN.W))
  )

  val rdata = Cat(
    mem(io.dmem.addr + 3.U(WORD_LEN.W)),
    mem(io.dmem.addr + 2.U(WORD_LEN.W)),
    mem(io.dmem.addr + 1.U(WORD_LEN.W)),
    mem(io.dmem.addr + 0.U(WORD_LEN.W))
  )
  io.dmem.rdatab := Cat(Fill(24, rdata(7)), rdata(7, 0))
  io.dmem.rdatabu := Cat(Fill(24, 0.U), rdata(7, 0))
  io.dmem.rdatah := Cat(Fill(16, rdata(15)), rdata(15, 0))
  io.dmem.rdatahu := Cat(Fill(16, 0.U), rdata(15, 0))
  io.dmem.rdataw := rdata

  when(io.dmem.wen === MenSel.B) {
    mem(io.dmem.addr + 0.U(WORD_LEN.W)) := io.dmem.wdata(7, 0)
  }.elsewhen(io.dmem.wen === MenSel.H) {
    mem(io.dmem.addr + 0.U(WORD_LEN.W)) := io.dmem.wdata(7, 0)
    mem(io.dmem.addr + 1.U(WORD_LEN.W)) := io.dmem.wdata(15, 8)
  }.elsewhen(io.dmem.wen === MenSel.W) {
    mem(io.dmem.addr + 0.U(WORD_LEN.W)) := io.dmem.wdata(7, 0)
    mem(io.dmem.addr + 1.U(WORD_LEN.W)) := io.dmem.wdata(15, 8)
    mem(io.dmem.addr + 2.U(WORD_LEN.W)) := io.dmem.wdata(23, 16)
    mem(io.dmem.addr + 3.U(WORD_LEN.W)) := io.dmem.wdata(31, 24)
  }
}
