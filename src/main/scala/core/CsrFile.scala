package core

import chisel3._
import chisel3.util._
import common.Consts._
import common.CsrCmd

object CsrAddr {
  val MIE = 0x304.U(CSR_ADDR_LEN.W)
  val MTVEC = 0x305.U(CSR_ADDR_LEN.W)
  val MEPC = 0x341.U(CSR_ADDR_LEN.W)
  val MCAUSE = 0x342.U(CSR_ADDR_LEN.W)
  val MIP = 0x344.U(CSR_ADDR_LEN.W)
  val CYCLE = 0xc00.U(CSR_ADDR_LEN.W)
}

object MipMask {
  val MTIP = (1 << 7).U(WORD_LEN.W)
}

object MieMask {
  val MTIE = (1 << 7).U(WORD_LEN.W)
}

class CsrIo extends Bundle {
  val rdata = Output(UInt(WORD_LEN.W))
  val wen = Input(Bool())
  val wdata = Input(UInt(CSR_ADDR_LEN.W))
}

class CsrFileIo extends Bundle {
  val cmd = Input(CsrCmd())
  val addr = Input(UInt(CSR_ADDR_LEN.W))
  val wdata = Input(UInt(WORD_LEN.W))
  val rdata = Output(UInt(WORD_LEN.W))

  val trap_valid = Input(Bool())
  val trap_code = Input(UInt(WORD_LEN.W))
  val trap_pc = Input(UInt(WORD_LEN.W))

  val mtvec = Output(UInt(WORD_LEN.W))
  val mepc = Output(UInt(WORD_LEN.W))
  val mtimecmp = new CsrIo()
  val mtime = new CsrIo()

  val interrupt_valid = Output(Bool())
  val interrupt_code = Output(UInt(WORD_LEN.W))
}

class CsrFile extends Module {
  val io = IO(new CsrFileIo())

  val regfile = Mem(CSR_NUM, UInt(WORD_LEN.W))
  val reg_mtimecmp = RegInit(0.U(WORD_LEN.W))
  val reg_mtime = RegInit(0.U(WORD_LEN.W))

  regfile(CsrAddr.CYCLE) := regfile(CsrAddr.CYCLE) + 1.U
  reg_mtime := reg_mtime + 1.U

  io.rdata := regfile(io.addr.asUInt)
  when(io.cmd =/= CsrCmd.X) {
    regfile(io.addr.asUInt) := MuxLookup(io.cmd, 0.U(WORD_LEN.W))(
      Seq(
        CsrCmd.W -> io.wdata,
        CsrCmd.S -> (io.rdata | io.wdata),
        CsrCmd.C -> (io.rdata & ~io.wdata)
      )
    )
  }

  when(io.trap_valid) {
    regfile(CsrAddr.MCAUSE) := io.trap_code
    regfile(CsrAddr.MEPC) := io.trap_pc
  }

  io.mtvec := regfile(CsrAddr.MTVEC)
  io.mepc := regfile(CsrAddr.MEPC)

  io.mtimecmp.rdata := reg_mtimecmp
  when(io.mtimecmp.wen) {
    reg_mtimecmp := io.mtimecmp.wdata
  }

  io.mtime.rdata := reg_mtime
  when(io.mtime.wen) {
    reg_mtime := io.mtime.wdata
  }

  when(reg_mtimecmp =/= 0.U(WORD_LEN.W) && reg_mtime >= reg_mtimecmp) {
    regfile(CsrAddr.MIP) := regfile(CsrAddr.MIP) | MipMask.MTIP
  }
}
