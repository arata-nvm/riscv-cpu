package core

import chisel3._
import chisel3.util._
import common.Consts._
import common.CsrCmd
import common.CsrPriv

object CsrAddr {
  val MSTATUS = 0x300.U(CSR_ADDR_LEN.W)
  val MISA = 0x301.U(CSR_ADDR_LEN.W)
  val MIE = 0x304.U(CSR_ADDR_LEN.W)
  val MTVEC = 0x305.U(CSR_ADDR_LEN.W)
  val MEPC = 0x341.U(CSR_ADDR_LEN.W)
  val MCAUSE = 0x342.U(CSR_ADDR_LEN.W)
  val MTVAL = 0x343.U(CSR_ADDR_LEN.W)
  val MIP = 0x344.U(CSR_ADDR_LEN.W)
  val CYCLE = 0xc00.U(CSR_ADDR_LEN.W)
  val MVENDORID = 0xf11.U(CSR_ADDR_LEN.W)
}

object CsrMstatusMask {
  val MIE_OFFSET = 3
  val MIE = (1 << MIE_OFFSET).U(WORD_LEN.W)
  val MPIE_OFFSET = 7
  val MPIE = (1 << MPIE_OFFSET).U(WORD_LEN.W)
  val MPP_OFFSET = 11
  val MPP = (3 << MPP_OFFSET).U(WORD_LEN.W)
}

object CsrMipMask {
  val MTIP = (1 << 7).U(WORD_LEN.W)
}

object CsrMieMask {
  val MTIE = (1 << 7).U(WORD_LEN.W)
}

class CsrReadRegsIo extends Bundle {
  val mstatus = Output(UInt(WORD_LEN.W))
  val mie = Output(UInt(WORD_LEN.W))
  val mtvec = Output(UInt(WORD_LEN.W))
  val mepc = Output(UInt(WORD_LEN.W))
  val mip = Output(UInt(WORD_LEN.W))
  val priv = Output(CsrPriv())
}

class CsrReadWriteRegsIo extends Bundle {
  val mtimecmp = new CsrReadWriteRegIo()
  val mtime = new CsrReadWriteRegIo()
}

class CsrReadWriteRegIo extends Bundle {
  val rdata = Output(UInt(WORD_LEN.W))
  val wen = Input(Bool())
  val wdata = Input(UInt(WORD_LEN.W))
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

  val ret = Input(Bool())
}

class CsrFileIo extends Bundle {
  val command = new CsrCommandIo()
  val regs_r = new CsrReadRegsIo()
  val regs_rw = new CsrReadWriteRegsIo()
  val trap = new CsrTrapIo()
}

class CsrFile extends Module {
  val io = IO(new CsrFileIo())

  val regfile = Mem(CSR_NUM, UInt(WORD_LEN.W))
  val reg_mtimecmp = RegInit(0.U(WORD_LEN.W))
  val reg_mtime = RegInit(0.U(WORD_LEN.W))
  val reg_mtime_sub = RegInit(0.U(WORD_LEN.W))
  val reg_priv = RegInit(CsrPriv.M)

  when(reset.asBool) {
    // always M-mode
    regfile(CsrAddr.MSTATUS) := regfile(CsrAddr.MSTATUS) | CsrMstatusMask.MPP
    regfile(CsrAddr.MVENDORID) := "x_ff0ff0ff".U(WORD_LEN.W)
    regfile(CsrAddr.MISA) := "x_40401101".U(WORD_LEN.W)
  }

  regfile(CsrAddr.CYCLE) := regfile(CsrAddr.CYCLE) + 1.U
  when(reg_mtime_sub === 3000.U(WORD_LEN.W)) {
    reg_mtime := reg_mtime + 1.U
    reg_mtime_sub := 0.U(WORD_LEN.W)
  }.otherwise {
    reg_mtime_sub := reg_mtime_sub + 1.U
  }

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
    printf("trap: pc = %x, cause = %x\n", io.trap.pc, io.trap.code)
    regfile(CsrAddr.MCAUSE) := io.trap.code
    regfile(CsrAddr.MEPC) := Mux(
      io.trap.code(WORD_LEN - 1) =/= 0.U(WORD_LEN.W),
      io.trap.pc + 4.U,
      io.trap.pc
    )
    regfile(CsrAddr.MTVAL) := 0.U(WORD_LEN.W)

    val mie = regfile(CsrAddr.MSTATUS)(CsrMstatusMask.MIE_OFFSET)
    regfile(CsrAddr.MSTATUS) := Cat(
      regfile(CsrAddr.MSTATUS)(WORD_LEN - 1, CsrMstatusMask.MPIE_OFFSET + 1),
      mie, // MPIE
      regfile(CsrAddr.MSTATUS)(
        CsrMstatusMask.MPIE_OFFSET - 1,
        CsrMstatusMask.MIE_OFFSET + 1
      ),
      0.U(1.W), // MIE
      regfile(CsrAddr.MSTATUS)(CsrMstatusMask.MIE_OFFSET - 1, 0)
    )

    reg_priv := CsrPriv.M
  }

  when(io.trap.ret) {
    printf("trap_ret: pc = %x, mepc = %x\n", io.trap.pc, io.regs_r.mepc)
    val mpie = regfile(CsrAddr.MSTATUS)(CsrMstatusMask.MPIE_OFFSET)
    regfile(CsrAddr.MSTATUS) := Cat(
      regfile(CsrAddr.MSTATUS)(WORD_LEN - 1, CsrMstatusMask.MPIE_OFFSET + 1),
      1.U(1.W), // MPIE
      regfile(CsrAddr.MSTATUS)(
        CsrMstatusMask.MPIE_OFFSET - 1,
        CsrMstatusMask.MIE_OFFSET + 1
      ),
      mpie, // MIE
      regfile(CsrAddr.MSTATUS)(CsrMstatusMask.MIE_OFFSET - 1, 0)
    )

    reg_priv := CsrPriv.U
  }

  io.regs_r.mstatus := regfile(CsrAddr.MSTATUS)
  io.regs_r.mie := regfile(CsrAddr.MIE)
  io.regs_r.mtvec := regfile(CsrAddr.MTVEC)
  io.regs_r.mepc := regfile(CsrAddr.MEPC)
  io.regs_r.mip := regfile(CsrAddr.MIP)
  io.regs_r.priv := reg_priv

  io.regs_rw.mtimecmp.rdata := reg_mtimecmp
  when(io.regs_rw.mtimecmp.wen) {
    reg_mtimecmp := io.regs_rw.mtimecmp.wdata
  }

  io.regs_rw.mtime.rdata := reg_mtime
  when(io.regs_rw.mtime.wen) {
    reg_mtime := io.regs_rw.mtime.wdata
  }

  when(reg_mtimecmp =/= 0.U(WORD_LEN.W) && reg_mtime >= reg_mtimecmp) {
    // printf("timer: mtime = %x, mtimecmp = %x\n", reg_mtime, reg_mtimecmp)
    regfile(CsrAddr.MIP) := regfile(CsrAddr.MIP) | CsrMipMask.MTIP
  }.otherwise {
    regfile(CsrAddr.MIP) := regfile(CsrAddr.MIP) & ~CsrMipMask.MTIP
  }
}
