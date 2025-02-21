package core

import chisel3._
import chisel3.util._
import common.Consts._

class CsrFileIo extends Bundle {
  val cmd = Input(UInt(WORD_LEN.W))
  val addr = Input(UInt(CSR_ADDR_LEN.W))
  val wdata = Input(UInt(WORD_LEN.W))
  val rdata = Output(UInt(WORD_LEN.W))

  val mtvec = Output(UInt(WORD_LEN.W))
}

class CsrFile extends Module {
  val io = IO(new CsrFileIo())

  val regfile = Mem(CSR_NUM, UInt(WORD_LEN.W))

  io.rdata := regfile(io.addr)
  when(io.cmd > 0.U(WORD_LEN.W)) {
    regfile(io.addr) := MuxLookup(io.cmd, 0.U(WORD_LEN.W))(
      Seq(
        CSR_W -> io.wdata,
        CSR_S -> (io.rdata | io.wdata),
        CSR_C -> (io.rdata & ~io.wdata),
        CSR_E -> 11.U(WORD_LEN.W)
      )
    )
  }

  io.mtvec := regfile(CSR_ADDR_MTVEC)
}
