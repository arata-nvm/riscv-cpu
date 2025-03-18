package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import common.Consts._

object MemSel extends ChiselEnum {
  val RAM, UART, CLINT, INVALID = Value
}

object MemRange {
  val RAM = BitPat("b100000??_????????_????????_????????")
  val RAM_MASK = 0x3ffffff.U(WORD_LEN.W)
  val UART = BitPat("b00010000_00000000_00000000_????????")
  val UART_MASK = 0xff.U(WORD_LEN.W)
  val CLINT = BitPat("b00010001_00000000_????????_????????")
  val CLINT_MASK = 0xffff.U(WORD_LEN.W)
}

class Bus extends Module {
  val io = IO(new Bundle {
    val dmem_in = new DmemPortIo()
    val dmem_out_memory = Flipped(new DmemPortIo())
    val dmem_out_uart = Flipped(new DmemPortIo())
    val dmem_out_clint = Flipped(new DmemPortIo())
  })

  def addr_to_sel(addr: UInt): MemSel.Type = {
    MuxCase(
      MemSel.INVALID,
      Seq(
        (addr === MemRange.RAM) -> MemSel.RAM,
        (addr === MemRange.UART) -> MemSel.UART,
        (addr === MemRange.CLINT) -> MemSel.CLINT
      )
    )
  }

  val rsel = addr_to_sel(io.dmem_in.raddr)
  val wsel = addr_to_sel(io.dmem_in.waddr)

  when(io.dmem_in.wen =/= MenSel.X && wsel === MemSel.INVALID) {
    printf(
      "[Bus] Invalid memory write at 0x%x = 0x%x\n",
      io.dmem_in.waddr,
      io.dmem_in.wdata
    )
  }

  io.dmem_out_memory.raddr := io.dmem_in.raddr & MemRange.RAM_MASK
  io.dmem_out_memory.wen := Mux(wsel === MemSel.RAM, io.dmem_in.wen, MenSel.X)
  io.dmem_out_memory.waddr := io.dmem_in.waddr & MemRange.RAM_MASK
  io.dmem_out_memory.wdata := io.dmem_in.wdata

  io.dmem_out_uart.raddr := io.dmem_in.raddr & MemRange.UART_MASK
  io.dmem_out_uart.wen := Mux(wsel === MemSel.UART, io.dmem_in.wen, MenSel.X)
  io.dmem_out_uart.waddr := io.dmem_in.waddr & MemRange.UART_MASK
  io.dmem_out_uart.wdata := io.dmem_in.wdata

  io.dmem_out_clint.raddr := io.dmem_in.raddr & MemRange.CLINT_MASK
  io.dmem_out_clint.wen := Mux(wsel === MemSel.CLINT, io.dmem_in.wen, MenSel.X)
  io.dmem_out_clint.waddr := io.dmem_in.waddr & MemRange.CLINT_MASK
  io.dmem_out_clint.wdata := io.dmem_in.wdata

  io.dmem_in.rdata := MuxLookup(rsel, 0.U(WORD_LEN.W))(
    Seq(
      MemSel.RAM -> io.dmem_out_memory.rdata,
      MemSel.UART -> io.dmem_out_uart.rdata,
      MemSel.CLINT -> io.dmem_out_clint.rdata
    )
  )
}
