package core

import chisel3._
import chisel3.util._
import common.Consts._

object ClintAddr {
  val MTIMECMP = 0x4000.U(WORD_LEN.W)
  val MTIME = 0xbff8.U(WORD_LEN.W)
}

class ClintMemory extends Module {
  val io = IO(new Bundle {
    val dmem = new DmemPortIo()
    val csrfile_regs_rw = Flipped(new CsrReadWriteRegsIo())
  })

  io.dmem.rdata := MuxLookup(io.dmem.raddr, 0.U(WORD_LEN.W))(
    Seq(
      ClintAddr.MTIMECMP -> io.csrfile_regs_rw.mtimecmp.rdata,
      ClintAddr.MTIME -> io.csrfile_regs_rw.mtime.rdata
    )
  )

  io.csrfile_regs_rw.mtimecmp.wdata := io.dmem.wdata
  io.csrfile_regs_rw.mtime.wdata := io.dmem.wdata

  val wen = io.dmem.wen === MenSel.W
  io.csrfile_regs_rw.mtimecmp.wen := wen && io.dmem.waddr === ClintAddr.MTIMECMP
  io.csrfile_regs_rw.mtime.wen := wen && io.dmem.waddr === ClintAddr.MTIME
}
