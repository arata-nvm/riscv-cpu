package core

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._
import core.pipeline.IfUnit
import core.pipeline.IdUnit
import core.pipeline.ExUnit
import core.pipeline.MeUnit
import core.pipeline.WbUnit

class Core(suppressLog: Boolean) extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new ImemPortIo())
    val dmem = Flipped(new DmemPortIo())
    val regfile = Flipped(new RegFileIo())
    val csrfile = Flipped(new CsrFileIo())
    val gp = Output(UInt(WORD_LEN.W))
    val pc = Output(UInt(WORD_LEN.W))
    val inst = Output(UInt(WORD_LEN.W))
  })

  val if_unit = Module(new IfUnit())
  val id_unit = Module(new IdUnit())
  val ex_unit = Module(new ExUnit())
  val me_unit = Module(new MeUnit())
  val wb_unit = Module(new WbUnit())

  if_unit.io.imem <> io.imem
  if_unit.io.if2id <> id_unit.io.if2id

  id_unit.io.regfile_rs1 <> io.regfile.rs1
  id_unit.io.regfile_rs2 <> io.regfile.rs2
  id_unit.io.id2if <> if_unit.io.id2if
  id_unit.io.id2ex <> ex_unit.io.id2ex

  ex_unit.io.csrfile_mtvec := io.csrfile.mtvec
  ex_unit.io.ex2if <> if_unit.io.ex2if
  ex_unit.io.ex2id <> id_unit.io.ex2id
  ex_unit.io.ex2me <> me_unit.io.ex2me

  me_unit.io.dmem <> io.dmem
  me_unit.io.csrfile <> io.csrfile
  me_unit.io.me2id <> id_unit.io.me2id
  me_unit.io.me2wb <> wb_unit.io.me2wb

  wb_unit.io.regfile_rd <> io.regfile.rd
  wb_unit.io.wb2id <> id_unit.io.wb2id

  // Debug

  io.gp := io.regfile.gp
  io.pc := ex_unit.io.ex2me.pc
  io.inst := ex_unit.io.ex2me.inst

  if (!suppressLog) {
    printf("--------------------\n")
    printf("--- if ---\n")
    printf(p"pc               : 0x${Hexadecimal(if_unit.io.if2id.pc)}\n")
    printf(p"inst             : 0x${Hexadecimal(if_unit.io.if2id.inst)}\n")
    printf(p"inst_id          : 0x${Hexadecimal(if_unit.io.if2id.inst_id)}\n")
    printf("--- id ---\n")
    printf(p"pc               : 0x${Hexadecimal(id_unit.io.id2ex.pc)}\n")
    printf(p"inst_id          : 0x${Hexadecimal(id_unit.io.id2ex.inst_id)}\n")
    printf(p"rs1_data         : 0x${Hexadecimal(id_unit.io.id2ex.rs1_data)}\n")
    printf(p"rs2_data         : 0x${Hexadecimal(id_unit.io.id2ex.rs2_data)}\n")
    printf(p"stall_flg        : 0x${Hexadecimal(id_unit.io.id2if.stall_flg)}\n")
    printf("--- ex ---\n")
    printf(p"pc               : 0x${Hexadecimal(ex_unit.io.ex2me.pc)}\n")
    printf(p"inst_id          : 0x${Hexadecimal(ex_unit.io.ex2me.inst_id)}\n")
    printf(p"reg_op1_data     : 0x${Hexadecimal(ex_unit.io.ex2me.op1_data)}\n")
    printf(p"reg_op2_data     : 0x${Hexadecimal(ex_unit.io.ex2me.op2_data)}\n")
    printf(p"alu_out          : 0x${Hexadecimal(ex_unit.io.ex2me.alu_out)}\n")
    printf("--- me ---\n")
    printf(p"pc               : 0x${Hexadecimal(me_unit.io.me2wb.pc)}\n")
    printf(p"inst_id          : 0x${Hexadecimal(me_unit.io.me2wb.inst_id)}\n")
    printf(p"wb_data          : 0x${Hexadecimal(me_unit.io.me2wb.wb_data)}\n")
    // printf(p"wb_inst_id       : 0x${Hexadecimal(wb_inst_id)}\n")
    // printf(p"wb_reg_wb_data   : 0x${Hexadecimal(wb_reg_wb_data)}\n")
    printf("---------\n")
  }
}
