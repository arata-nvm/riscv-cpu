package core

import chisel3._
import chisel3.util._
import common.Consts._
import common.CsrCmd
import common.Consts
import core.Counter

class CsrFileIo extends Bundle {
  val cmd = Input(CsrCmd())
  val addr = Input(UInt(CSR_ADDR_LEN.W))
  val wdata = Input(UInt(WORD_LEN.W))
  val rdata = Output(UInt(WORD_LEN.W))

  val mtvec = Output(UInt(WORD_LEN.W))
}

class CsrFile extends Module {
  val io = IO(new CsrFileIo())

  val regfile = Mem(CSR_NUM, UInt(WORD_LEN.W))
  val counter = Module(new Counter())

  io.rdata := Mux(
    io.addr.asUInt === CSR_ADDR_CYCLE,
    counter.io.counter,
    regfile(io.addr.asUInt)
  )
  when(io.cmd =/= CsrCmd.X) {
    regfile(io.addr.asUInt) := MuxLookup(io.cmd, 0.U(WORD_LEN.W))(
      Seq(
        CsrCmd.W -> io.wdata,
        CsrCmd.S -> (io.rdata | io.wdata),
        CsrCmd.C -> (io.rdata & ~io.wdata),
        CsrCmd.E -> 11.U(WORD_LEN.W)
      )
    )
  }

  io.mtvec := regfile(CSR_ADDR_MTVEC.asUInt)
}
