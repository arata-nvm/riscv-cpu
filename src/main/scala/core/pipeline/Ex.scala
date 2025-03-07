package core.pipeline

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._
import common.WbSel
import common.RenSel
import common.CsrCmd
import core.MenSel
import common.ExFunc
import core.CsrFileIo

class Ex2IfIo extends Bundle {
  val branch_taken = Output(Bool())
  val branch_target = Output(UInt(WORD_LEN.W))
}

class Ex2IdIo extends Bundle {
  val branch_taken = Output(Bool())
}

class Ex2MeIo extends Bundle {
  val pc = Output(UInt(WORD_LEN.W))
  val inst_id = Output(UInt(WORD_LEN.W))
  val op1_data = Output(UInt(WORD_LEN.W))
  val op2_data = Output(UInt(WORD_LEN.W))
  val rs2_data = Output(UInt(WORD_LEN.W))
  val imm_z_uext = Output(UInt(WORD_LEN.W))
  val alu_out = Output(UInt(WORD_LEN.W))
  val mem_wen = Output(MenSel())
  val csr_cmd = Output(CsrCmd())
  val csr_addr = Output(UInt(WORD_LEN.W))
  val rf_wen = Output(RenSel())
  val wb_sel = Output(WbSel())
  val wb_addr = Output(UInt(WORD_LEN.W))
}

class ExUnit extends Module {
  val io = IO(new Bundle {
    val csrfile_mtvec = Input(UInt(WORD_LEN.W))
    val id2ex = Flipped(new Id2ExIo())
    val ex2if = new Ex2IfIo()
    val ex2id = new Ex2IdIo()
    val ex2me = new Ex2MeIo()
  })

  val alu_out = MuxLookup(io.id2ex.exe_fun, 0.U(WORD_LEN.W))(
    Seq(
      ExFunc.ADD -> (io.id2ex.op1_data + io.id2ex.op2_data),
      ExFunc.SUB -> (io.id2ex.op1_data - io.id2ex.op2_data),
      ExFunc.AND -> (io.id2ex.op1_data & io.id2ex.op2_data),
      ExFunc.OR -> (io.id2ex.op1_data | io.id2ex.op2_data),
      ExFunc.XOR -> (io.id2ex.op1_data ^ io.id2ex.op2_data),
      ExFunc.SLL -> (io.id2ex.op1_data << io.id2ex
        .op2_data(4, 0))(WORD_LEN - 1, 0),
      ExFunc.SRL -> (io.id2ex.op1_data >> io.id2ex.op2_data(4, 0)).asUInt,
      ExFunc.SRA -> (io.id2ex.op1_data.asSInt >> io.id2ex
        .op2_data(4, 0)).asUInt,
      ExFunc.SLT -> (io.id2ex.op1_data.asSInt < io.id2ex.op2_data.asSInt).asUInt,
      ExFunc.SLTU -> (io.id2ex.op1_data < io.id2ex.op2_data).asUInt,
      ExFunc.JALR -> ((io.id2ex.op1_data + io.id2ex.op2_data) & ~1.U(
        WORD_LEN.W
      )),
      ExFunc.COPY1 -> io.id2ex.op1_data,
      ExFunc.PCNT -> PopCount(io.id2ex.op1_data),
      ExFunc.MUL -> (io.id2ex.op1_data * io.id2ex.op2_data),
      ExFunc.MULH -> (io.id2ex.op1_data.asSInt * io.id2ex.op2_data.asSInt)(
        WORD_LEN * 2 - 1,
        WORD_LEN
      ).asUInt,
      ExFunc.MULHU -> (io.id2ex.op1_data.asUInt * io.id2ex.op2_data.asUInt)(
        WORD_LEN * 2 - 1,
        WORD_LEN
      ).asUInt,
      ExFunc.MULHSU -> (io.id2ex.op1_data.asSInt * io.id2ex.op2_data.asUInt)(
        WORD_LEN * 2 - 1,
        WORD_LEN
      ).asUInt,
      ExFunc.DIV -> Mux(
        io.id2ex.op2_data === 0.U(WORD_LEN.W),
        ~0.U(WORD_LEN.W),
        (io.id2ex.op1_data.asSInt / io.id2ex.op2_data.asSInt).asUInt
      ),
      ExFunc.DIVU -> Mux(
        io.id2ex.op2_data === 0.U(WORD_LEN.W),
        ~0.U(WORD_LEN.W),
        (io.id2ex.op1_data / io.id2ex.op2_data)
      ),
      ExFunc.REM -> Mux(
        io.id2ex.op2_data === 0.U(WORD_LEN.W),
        io.id2ex.op1_data,
        (io.id2ex.op1_data.asSInt % io.id2ex.op2_data.asSInt).asUInt
      ),
      ExFunc.REMU -> Mux(
        io.id2ex.op2_data === 0.U(WORD_LEN.W),
        io.id2ex.op1_data,
        (io.id2ex.op1_data % io.id2ex.op2_data)
      )
    )
  )

  val br_flg = MuxLookup(io.id2ex.exe_fun, false.B)(
    Seq(
      ExFunc.BEQ -> (io.id2ex.op1_data === io.id2ex.op2_data),
      ExFunc.BNE -> !(io.id2ex.op1_data === io.id2ex.op2_data),
      ExFunc.BLT -> (io.id2ex.op1_data.asSInt < io.id2ex.op2_data.asSInt),
      ExFunc.BGE -> !(io.id2ex.op1_data.asSInt < io.id2ex.op2_data.asSInt),
      ExFunc.BLTU -> (io.id2ex.op1_data < io.id2ex.op2_data),
      ExFunc.BGEU -> !(io.id2ex.op1_data < io.id2ex.op2_data)
    )
  )
  val br_target = io.id2ex.pc + io.id2ex.imm_b_sext

  val jmp_flg = (io.id2ex.wb_sel === WbSel.PC)
  val jmp_target = alu_out

  val ecall_flg = (io.id2ex.exe_fun === ExFunc.ECALL)
  val ecall_target = io.csrfile_mtvec

  val branch_taken = br_flg || jmp_flg || ecall_flg
  val branch_target = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      br_flg -> br_target,
      jmp_flg -> jmp_target,
      ecall_flg -> ecall_target
    )
  )

  io.ex2if.branch_taken := branch_taken
  io.ex2if.branch_target := branch_target
  io.ex2id.branch_taken := branch_taken
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
