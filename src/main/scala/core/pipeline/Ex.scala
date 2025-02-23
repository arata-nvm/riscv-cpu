package core.pipeline

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._

class Ex2IfIo extends Bundle {
  val br_flg = Output(Bool())
  val br_target = Output(UInt(WORD_LEN.W))
  val jmp_flg = Output(Bool())
  val alu_out = Output(UInt(WORD_LEN.W))
}

class Ex2IdIo extends Bundle {
  val br_flg = Output(Bool())
  val jmp_flg = Output(Bool())
}

class Ex2MeIo extends Bundle {
  val pc = Output(UInt(WORD_LEN.W))
  val inst_id = Output(UInt(WORD_LEN.W))
  val op1_data = Output(UInt(WORD_LEN.W))
  val op2_data = Output(UInt(WORD_LEN.W))
  val rs2_data = Output(UInt(WORD_LEN.W))
  val imm_z_uext = Output(UInt(WORD_LEN.W))
  val alu_out = Output(UInt(WORD_LEN.W))
  val mem_wen = Output(UInt(WORD_LEN.W))
  val csr_cmd = Output(UInt(WORD_LEN.W))
  val csr_addr = Output(UInt(WORD_LEN.W))
  val rf_wen = Output(UInt(WORD_LEN.W))
  val wb_sel = Output(UInt(WORD_LEN.W))
  val wb_addr = Output(UInt(WORD_LEN.W))
}

class ExUnit extends Module {
  val io = IO(new Bundle {
    val id2ex = Flipped(new Id2ExIo())
    val ex2if = new Ex2IfIo()
    val ex2id = new Ex2IdIo()
    val ex2me = new Ex2MeIo()
  })

  val alu_out = MuxLookup(io.id2ex.exe_fun, 0.U(WORD_LEN.W))(
    Seq(
      ALU_ADD -> (io.id2ex.op1_data + io.id2ex.op2_data),
      ALU_SUB -> (io.id2ex.op1_data - io.id2ex.op2_data),
      ALU_AND -> (io.id2ex.op1_data & io.id2ex.op2_data),
      ALU_OR -> (io.id2ex.op1_data | io.id2ex.op2_data),
      ALU_XOR -> (io.id2ex.op1_data ^ io.id2ex.op2_data),
      ALU_SLL -> (io.id2ex.op1_data << io.id2ex.op2_data(4, 0))(31, 0),
      ALU_SRL -> (io.id2ex.op1_data >> io.id2ex.op2_data(4, 0)).asUInt,
      ALU_SRA -> (io.id2ex.op1_data.asSInt >> io.id2ex.op2_data(4, 0)).asUInt,
      ALU_SLT -> (io.id2ex.op1_data.asSInt < io.id2ex.op2_data.asSInt).asUInt,
      ALU_SLTU -> (io.id2ex.op1_data < io.id2ex.op2_data).asUInt,
      ALU_JALR -> ((io.id2ex.op1_data + io.id2ex.op2_data) & ~1.U(WORD_LEN.W)),
      ALU_COPY1 -> io.id2ex.op1_data,
      ALU_PCNT -> PopCount(io.id2ex.op1_data)
    )
  )

  val br_flg = MuxLookup(io.id2ex.exe_fun, false.B)(
    Seq(
      BR_BEQ -> (io.id2ex.op1_data === io.id2ex.op2_data),
      BR_BNE -> !(io.id2ex.op1_data === io.id2ex.op2_data),
      BR_BLT -> (io.id2ex.op1_data.asSInt < io.id2ex.op2_data.asSInt),
      BR_BGE -> !(io.id2ex.op1_data.asSInt < io.id2ex.op2_data.asSInt),
      BR_BLTU -> (io.id2ex.op1_data < io.id2ex.op2_data),
      BR_BGEU -> !(io.id2ex.op1_data < io.id2ex.op2_data)
    )
  )
  val br_target = io.id2ex.pc + io.id2ex.imm_b_sext

  val jmp_flg = (io.id2ex.wb_sel === WB_PC)

  io.ex2if.br_flg := br_flg
  io.ex2if.br_target := br_target
  io.ex2if.jmp_flg := jmp_flg
  io.ex2if.alu_out := alu_out
  io.ex2id.br_flg := br_flg
  io.ex2id.br_target := br_target
  io.ex2id.jmp_flg := jmp_flg
  io.ex2id.alu_out := alu_out
  io.ex2me.pc := RegNext(io.id2ex.pc)
  io.ex2me.inst_id := RegNext(io.id2ex.inst_id)
  io.ex2me.op1_data := RegNext(io.id2ex.op1_data)
  io.ex2me.op2_data := RegNext(io.id2ex.op2_data)
  io.ex2me.rs2_data := RegNext(io.id2ex.rs2_data)
  io.ex2me.imm_z_uext := RegNext(io.id2ex.imm_z_uext)
  io.ex2me.alu_out := RegNext(alu_out)
  io.ex2me.mem_wen := RegNext(io.id2ex.mem_wen)
  io.ex2me.csr_cmd := RegNext(io.id2ex.csr_cmd)
  io.ex2me.csr_addr := RegNext(io.id2ex.csr_addr)
  io.ex2me.rf_wen := RegNext(io.id2ex.rf_wen)
  io.ex2me.wb_sel := RegNext(io.id2ex.wb_sel)
  io.ex2me.wb_addr := RegNext(io.id2ex.wb_addr)
}
