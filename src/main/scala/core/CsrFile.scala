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

class CsrReadRegsIo extends Bundle {
  val mtvec = Output(UInt(WORD_LEN.W))
  val mepc = Output(UInt(WORD_LEN.W))
}

class CsrReadWriteRegsIo extends Bundle {
  val mtimecmp = new CsrReadWriteRegIo()
  val mtime = new CsrReadWriteRegIo()
}

class CsrReadWriteRegIo extends Bundle {
  val rdata = Output(UInt(WORD_LEN.W))
  val wen = Input(Bool())
  val wdata = Input(UInt(CSR_ADDR_LEN.W))
}

class CsrCommandIo extends Bundle {
  val cmd = Input(CsrCmd())
  val addr = Input(UInt(CSR_ADDR_LEN.W))
  val wdata = Input(UInt(WORD_LEN.W))
  val rdata = Output(UInt(WORD_LEN.W))
}

class CsrTrapIo extends Bundle {
  val valid = Input(Bool())
  val code = Input(UInt(WORD_LEN.W))
  val pc = Input(UInt(WORD_LEN.W))
}

class CsrInterruptIo extends Bundle {
  val valid = Output(Bool())
  val code = Output(UInt(WORD_LEN.W))
}

class CsrFileIo extends Bundle {
  val command = new CsrCommandIo()
  val regs_r = new CsrReadRegsIo()
  val regs_rw = new CsrReadWriteRegsIo()
  val trap = new CsrTrapIo()
  // val interrupt = new CsrInterruptIo()
}

class CsrFile extends Module {
  val io = IO(new CsrFileIo())

  val regfile = Mem(CSR_NUM, UInt(WORD_LEN.W))
  val reg_mtimecmp = RegInit(0.U(WORD_LEN.W))
  val reg_mtime = RegInit(0.U(WORD_LEN.W))

  regfile(CsrAddr.CYCLE) := regfile(CsrAddr.CYCLE) + 1.U
  reg_mtime := reg_mtime + 1.U

  io.command.rdata := regfile(io.command.addr.asUInt)
  when(io.command.cmd =/= CsrCmd.X) {
    regfile(io.command.addr.asUInt) := MuxLookup(
      io.command.cmd,
      0.U(WORD_LEN.W)
    )(
      Seq(
        CsrCmd.W -> io.command.wdata,
        CsrCmd.S -> (io.command.rdata | io.command.wdata),
        CsrCmd.C -> (io.command.rdata & ~io.command.wdata)
      )
    )
  }

  when(io.trap.valid) {
    regfile(CsrAddr.MCAUSE) := io.trap.code
    regfile(CsrAddr.MEPC) := io.trap.pc
  }

  io.regs_r.mtvec := regfile(CsrAddr.MTVEC)
  io.regs_r.mepc := regfile(CsrAddr.MEPC)

  io.regs_rw.mtimecmp.rdata := reg_mtimecmp
  when(io.regs_rw.mtimecmp.wen) {
    reg_mtimecmp := io.regs_rw.mtimecmp.wdata
  }

  io.regs_rw.mtime.rdata := reg_mtime
  when(io.regs_rw.mtime.wen) {
    reg_mtime := io.regs_rw.mtime.wdata
  }

  when(reg_mtimecmp =/= 0.U(WORD_LEN.W) && reg_mtime >= reg_mtimecmp) {
    regfile(CsrAddr.MIP) := regfile(CsrAddr.MIP) | MipMask.MTIP
  }
}
