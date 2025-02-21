package core

import chisel3._
import chisel3.util._
import common.Consts._

class RegFileIo extends Bundle {
  val rs1_addr = Input(UInt(REG_ADDR_LEN.W))
  val rs1_data = Output(UInt(WORD_LEN.W))

  val rs2_addr = Input(UInt(REG_ADDR_LEN.W))
  val rs2_data = Output(UInt(WORD_LEN.W))

  val rd_wen = Input(Bool())
  val rd_addr = Input(UInt(REG_ADDR_LEN.W))
  val rd_data = Input(UInt(WORD_LEN.W))

  val gp = Output(UInt(WORD_LEN.W))
}

class RegFile extends Module {
  val io = IO(new RegFileIo())

  val regfile = Mem(REG_NUM, UInt(WORD_LEN.W))

  io.rs1_data := regfile(io.rs1_addr)
  io.rs2_data := regfile(io.rs2_addr)
  when(io.rd_wen) {
    regfile(io.rd_addr) := io.rd_data
  }
  io.gp := regfile(3)
}
