package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import common.Consts._

object MemSel extends ChiselEnum {
  val MEMORY, UART = Value
}

object MemRange {
  val UART = BitPat("b00010000_00000000_00000000_000000??")
}

class MmioController extends Module {
  val io = IO(new Bundle {
    val dmem_in = new DmemPortIo()
    val dmem_out_memory = Flipped(new DmemPortIo())
    val dmem_out_uart = Flipped(new DmemPortIo())
  })

  val sel = MuxCase(
    MemSel.MEMORY,
    Seq((io.dmem_in.addr === MemRange.UART) -> MemSel.UART)
  )
  io.dmem_out_memory.addr := io.dmem_in.addr
  io.dmem_out_memory.wen := Mux(sel === MemSel.MEMORY, io.dmem_in.wen, MenSel.X)
  io.dmem_out_memory.wdata := io.dmem_in.wdata

  io.dmem_out_uart.addr := io.dmem_in.addr
  io.dmem_out_uart.wen := Mux(sel === MemSel.UART, io.dmem_in.wen, MenSel.X)
  io.dmem_out_uart.wdata := io.dmem_in.wdata

  io.dmem_in.rdatab := Mux(
    sel === MemSel.UART,
    io.dmem_out_uart.rdatab,
    io.dmem_out_memory.rdatab
  )
  io.dmem_in.rdatabu := Mux(
    sel === MemSel.UART,
    io.dmem_out_uart.rdatabu,
    io.dmem_out_memory.rdatabu
  )
  io.dmem_in.rdatah := Mux(
    sel === MemSel.UART,
    io.dmem_out_uart.rdatah,
    io.dmem_out_memory.rdatah
  )
  io.dmem_in.rdatahu := Mux(
    sel === MemSel.UART,
    io.dmem_out_uart.rdatahu,
    io.dmem_out_memory.rdatahu
  )
  io.dmem_in.rdataw := Mux(
    sel === MemSel.UART,
    io.dmem_out_uart.rdataw,
    io.dmem_out_memory.rdataw
  )
}
