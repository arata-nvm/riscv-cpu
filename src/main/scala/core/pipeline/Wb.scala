package core.pipeline

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._
import core.RegFileWriterIo
import common.RenSel

class Wb2IdIo extends Bundle {
  val rf_wen = Output(RenSel())
  val wb_addr = Output(UInt(ADDR_LEN.W))
  val wb_data = Output(UInt(WORD_LEN.W))
}

class WbUnit extends Module {
  val io = IO(new Bundle {
    val regfile_rd = Flipped(new RegFileWriterIo())
    val wb2id = new Wb2IdIo()
    val me2wb = Flipped(new Me2WbIo())
  })

  io.regfile_rd.wen := (io.me2wb.rf_wen === RenSel.S)
  io.regfile_rd.addr := io.me2wb.wb_addr
  io.regfile_rd.data := io.me2wb.wb_data

  io.wb2id.rf_wen := io.me2wb.rf_wen
  io.wb2id.wb_addr := io.me2wb.wb_addr
  io.wb2id.wb_data := io.me2wb.wb_data
}
