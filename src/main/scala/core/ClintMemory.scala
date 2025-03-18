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
    val csrfile_mtimecmp = Flipped(new CsrIo())
    val csrfile_mtime = Flipped(new CsrIo())
  })

  io.dmem.rdata := MuxLookup(io.dmem.raddr, 0.U(WORD_LEN.W))(
    Seq(
      ClintAddr.MTIMECMP -> io.csrfile_mtimecmp.rdata,
      ClintAddr.MTIME -> io.csrfile_mtime.rdata
    )
  )

  io.csrfile_mtimecmp.wdata := io.dmem.wdata
  io.csrfile_mtime.wdata := io.dmem.wdata

  val wen = io.dmem.wen === MenSel.W
  io.csrfile_mtimecmp.wen := wen && io.dmem.waddr === ClintAddr.MTIMECMP
  io.csrfile_mtime.wen := wen && io.dmem.waddr === ClintAddr.MTIME
}
