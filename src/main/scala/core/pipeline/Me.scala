package core.pipeline

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._
import core.DmemPortIo
import core.CsrFileIo
import common.RenSel
import common.WbSel
import common.MenSel

class Me2IdIo extends Bundle {
  val rf_wen = Output(RenSel())
  val wb_addr = Output(UInt(ADDR_LEN.W))
  val wb_data = Output(UInt(WORD_LEN.W))
}

class Me2WbIo extends Bundle {
  val pc = Output(UInt(WORD_LEN.W))
  val inst_id = Output(UInt(WORD_LEN.W))
  val rf_wen = Output(RenSel())
  val wb_addr = Output(UInt(ADDR_LEN.W))
  val wb_data = Output(UInt(WORD_LEN.W))
}

class MeUnit extends Module {
  val io = IO(new Bundle {
    val dmem = Flipped(new DmemPortIo())
    val csrfile = Flipped(new CsrFileIo())
    val ex2me = Flipped(new Ex2MeIo())
    val me2id = new Me2IdIo()
    val me2wb = new Me2WbIo()
  })

  io.dmem.addr := io.ex2me.alu_out
  io.dmem.wen := io.ex2me.mem_wen === MenSel.S
  io.dmem.wdata := io.ex2me.rs2_data

  io.csrfile.cmd := io.ex2me.csr_cmd
  io.csrfile.addr := io.ex2me.csr_addr
  io.csrfile.wdata := io.ex2me.op1_data

  val mem_pc_plus4 = io.ex2me.pc + 4.U(WORD_LEN.W)
  val wb_data = MuxLookup(io.ex2me.wb_sel, io.ex2me.alu_out)(
    Seq(
      WbSel.MEM -> io.dmem.rdata,
      WbSel.PC -> mem_pc_plus4,
      WbSel.CSR -> io.csrfile.rdata
    )
  )

  io.me2id.rf_wen := io.ex2me.rf_wen
  io.me2id.wb_addr := io.ex2me.wb_addr
  io.me2id.wb_data := wb_data
  io.me2wb.pc := RegNext(io.ex2me.pc)
  io.me2wb.inst_id := RegNext(io.ex2me.inst_id)
  io.me2wb.rf_wen := RegNext(io.ex2me.rf_wen)
  io.me2wb.wb_addr := RegNext(io.ex2me.wb_addr)
  io.me2wb.wb_data := RegNext(wb_data)
}
