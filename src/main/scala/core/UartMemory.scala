package core

import chisel3._
import chisel3.util._
import common.Consts._

object UartAddr {
  val RBR = 0x00.U(WORD_LEN.W) // Receive Buffer Register
  val LSR = 0x05.U(WORD_LEN.W) // Line Status Register
}

class UartMemory extends Module {
  val io = IO(new Bundle {
    val dmem = new DmemPortIo()
  })

  io.dmem.rdata := MuxLookup(io.dmem.raddr, 0.U(WORD_LEN.W))(
    Seq(
      UartAddr.LSR -> 0x60.U(WORD_LEN.W)
    )
  )

  when(io.dmem.wen === MenSel.B && io.dmem.waddr === UartAddr.RBR) {
    printf("%c", io.dmem.wdata(7, 0))
  }
}
