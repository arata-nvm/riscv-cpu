package core.pipeline

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._
import core.ImemPortIo

class If2IdIo extends Bundle {
  val pc = Output(UInt(WORD_LEN.W))
  val inst = Output(UInt(WORD_LEN.W))
  val inst_id = Output(UInt(WORD_LEN.W))
}

class IfUnit extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new ImemPortIo())
    val csrfile_mtvec = Input(UInt(WORD_LEN.W))
    val if2id = new If2IdIo()
    val id2if = Flipped(new Id2IfIo())
    val ex2if = Flipped(new Ex2IfIo())
  })

  val reg_pc = RegInit(START_ADDR)
  val reg_prev_pc = RegInit(START_ADDR)
  val reg_inst = RegInit(0.U(WORD_LEN.W))
  val reg_inst_id = RegInit(~0.U(WORD_LEN.W))

  io.imem.addr := reg_pc
  val inst = io.imem.inst

  val pc_plus4 = reg_pc + INST_LEN
  val pc_next = MuxCase(
    pc_plus4,
    Seq(
      io.ex2if.br_flg -> io.ex2if.br_target,
      io.ex2if.jmp_flg -> io.ex2if.alu_out,
      (inst === ECALL) -> io.csrfile_mtvec,
      io.id2if.stall_flg -> reg_pc
    )
  )
  reg_pc := pc_next
  reg_prev_pc := Mux(io.id2if.stall_flg, reg_prev_pc, reg_pc)

  reg_inst := MuxCase(
    inst,
    Seq(
      (io.ex2if.br_flg || io.ex2if.jmp_flg) -> BUBBLE,
      io.id2if.stall_flg -> reg_inst
    )
  )

  reg_inst_id := Mux(io.id2if.stall_flg, reg_inst_id, reg_inst_id + 1.U)

  io.if2id.pc := reg_prev_pc
  io.if2id.inst := reg_inst
  io.if2id.inst_id := reg_inst_id
}
