package core

import chisel3._
import chisel3.util._
import common.Consts._

class RegFileReaderIo extends Bundle {
  val addr = Input(UInt(REG_ADDR_LEN.W))
  val data = Output(UInt(WORD_LEN.W))
}

class RegFileWriterIo extends Bundle {
  val wen = Input(Bool())
  val addr = Input(UInt(REG_ADDR_LEN.W))
  val data = Input(UInt(WORD_LEN.W))
}

class RegFileIo extends Bundle {
  val rs1 = new RegFileReaderIo()
  val rs2 = new RegFileReaderIo()
  val rd = new RegFileWriterIo()
  val gp = Output(UInt(WORD_LEN.W))
}

object Registers {
  val R0 = 10
  val R1 = 11
}

class RegFile extends Module {
  val io = IO(new RegFileIo())

  val regfile = Mem(REG_NUM, UInt(WORD_LEN.W))

  when(reset.asBool) {
    regfile(Registers.R0) := 0.U(WORD_LEN.W) // hart ID
    regfile(Registers.R1) := 0.U(WORD_LEN.W) // pointer to dtb
  }

  io.rs1.data := regfile(io.rs1.addr)
  io.rs2.data := regfile(io.rs2.addr)
  when(io.rd.wen) {
    regfile(io.rd.addr) := io.rd.data
  }
  io.gp := regfile(3)
}
