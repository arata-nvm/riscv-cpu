package core

import chisel3._
import chisel3.util._
import common.Consts._

object UartAddr {
  val UART_TX_DATA = 0x10000000.U(WORD_LEN.W)
}

class UartMemory extends Module {
  val io = IO(new Bundle {
    val dmem = new DmemPortIo()
  })

  io.dmem.rdata := 0.U(WORD_LEN.W)

  when(io.dmem.wen === MenSel.B && io.dmem.waddr === UartAddr.UART_TX_DATA) {
    printf("%c", io.dmem.wdata(7, 0))
  }
}
