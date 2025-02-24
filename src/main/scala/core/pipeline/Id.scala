package core.pipeline

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._
import core.ImemPortIo
import core.RegFileReaderIo

class Id2IfIo extends Bundle {
  val stall_flg = Output(Bool())
}

class Id2ExIo extends Bundle {
  val pc = Output(UInt(WORD_LEN.W))
  val inst_id = Output(UInt(WORD_LEN.W))
  val op1_data = Output(UInt(WORD_LEN.W))
  val op2_data = Output(UInt(WORD_LEN.W))
  val rs1_data = Output(UInt(WORD_LEN.W))
  val rs2_data = Output(UInt(WORD_LEN.W))
  val imm_i_sext = Output(UInt(WORD_LEN.W))
  val imm_s_sext = Output(UInt(WORD_LEN.W))
  val imm_b_sext = Output(UInt(WORD_LEN.W))
  val imm_u_shifted = Output(UInt(WORD_LEN.W))
  val imm_z_uext = Output(UInt(WORD_LEN.W))
  val exe_fun = Output(UInt(EXE_FUN_LEN.W))
  val mem_wen = Output(UInt(MEN_LEN.W))
  val csr_cmd = Output(UInt(CSR_LEN.W))
  val csr_addr = Output(UInt(CSR_ADDR_LEN.W))
  val rf_wen = Output(UInt(REN_LEN.W))
  val wb_sel = Output(UInt(WB_SEL_LEN.W))
  val wb_addr = Output(UInt(ADDR_LEN.W))
}

class IdUnit extends Module {
  val io = IO(new Bundle {
    val regfile_rs1 = Flipped(new RegFileReaderIo())
    val regfile_rs2 = Flipped(new RegFileReaderIo())
    val if2id = Flipped(new If2IdIo())
    val id2if = new Id2IfIo()
    val id2ex = new Id2ExIo()
    val ex2id = Flipped(new Ex2IdIo())
    val me2id = Flipped(new Me2IdIo())
    val wb2id = Flipped(new Wb2IdIo())
  })

  val reg_rf_wen = RegInit(0.U(REN_LEN.W))
  val reg_wb_addr = RegInit(0.U(ADDR_LEN.W))

  val rs1_addr_b = io.if2id.inst(19, 15)
  val rs2_addr_b = io.if2id.inst(24, 20)
  val rs1_data_hazard =
    reg_rf_wen === REN_S && rs1_addr_b =/= 0.U && rs1_addr_b === reg_wb_addr
  val rs2_data_hazard =
    reg_rf_wen === REN_S && rs2_addr_b =/= 0.U && rs2_addr_b === reg_wb_addr
  val stall_flg = rs1_data_hazard || rs2_data_hazard

  val inst = Mux(
    io.ex2id.br_flg || io.ex2id.jmp_flg || stall_flg,
    BUBBLE,
    io.if2id.inst
  )
  val inst_id = Mux(
    io.ex2id.br_flg || io.ex2id.jmp_flg || stall_flg,
    ~0.U(WORD_LEN.W),
    io.if2id.inst_id
  )

  val rs1_addr = inst(19, 15)
  val rs2_addr = inst(24, 20)
  val wb_addr = inst(11, 7)

  io.regfile_rs1.addr := rs1_addr
  val rs1_data =
    MuxCase(
      io.regfile_rs1.data,
      Seq(
        (rs1_addr === 0.U) -> 0.U(WORD_LEN.W),
        (rs1_addr === io.me2id.wb_addr && io.me2id.rf_wen === REN_S) -> io.me2id.wb_data,
        (rs1_addr === io.wb2id.wb_addr && io.wb2id.rf_wen === REN_S) -> io.wb2id.wb_data
      )
    )
  io.regfile_rs2.addr := rs2_addr
  val rs2_data =
    MuxCase(
      io.regfile_rs2.data,
      Seq(
        (rs2_addr === 0.U) -> 0.U(WORD_LEN.W),
        (rs2_addr === io.me2id.wb_addr && io.me2id.rf_wen === REN_S) -> io.me2id.wb_data,
        (rs2_addr === io.wb2id.wb_addr && io.wb2id.rf_wen === REN_S) -> io.wb2id.wb_data
      )
    )

  val imm_i = inst(31, 20)
  val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i)
  val imm_s = Cat(inst(31, 25), inst(11, 7))
  val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)
  val imm_b = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8))
  val imm_b_sext = Cat(Fill(19, imm_b(11)), imm_b, 0.U(1.W))
  val imm_j = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21))
  val imm_j_sext = Cat(Fill(11, imm_j(19)), imm_j, 0.U(1.W))
  val imm_u = inst(31, 12)
  val imm_u_shifted = Cat(imm_u, Fill(12, 0.U))
  val imm_z = inst(19, 15)
  val imm_z_uext = Cat(Fill(27, 0.U), imm_z)

  val csignals = ListLookup(
    inst,
    List(ALU_X, OP1_X, OP2_X, MEN_X, REN_X, WB_X, CSR_X),
    Array(
      LW -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM, CSR_X),
      SW -> List(ALU_ADD, OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X, CSR_X),
      ADD -> List(ALU_ADD, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      ADDI -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SUB -> List(ALU_SUB, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      AND -> List(ALU_AND, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      OR -> List(ALU_OR, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      XOR -> List(ALU_XOR, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      ANDI -> List(ALU_AND, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      ORI -> List(ALU_OR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      XORI -> List(ALU_XOR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SLL -> List(ALU_SLL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SRL -> List(ALU_SRL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SRA -> List(ALU_SRA, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SLLI -> List(ALU_SLL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SRLI -> List(ALU_SRL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SRAI -> List(ALU_SRA, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SLT -> List(ALU_SLT, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SLTU -> List(ALU_SLTU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SLTI -> List(ALU_SLT, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SLTIU -> List(ALU_SLTU, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      BEQ -> List(BR_BEQ, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BNE -> List(BR_BNE, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BGE -> List(BR_BGE, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BGEU -> List(BR_BGEU, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BLT -> List(BR_BLT, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BLTU -> List(BR_BLTU, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      JAL -> List(ALU_ADD, OP1_PC, OP2_IMJ, MEN_X, REN_S, WB_PC, CSR_X),
      JALR -> List(ALU_JALR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC, CSR_X),
      LUI -> List(ALU_ADD, OP1_X, OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),
      AUIPC -> List(ALU_ADD, OP1_PC, OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),
      CSRRW -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_W),
      CSRRWI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_W),
      CSRRS -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_S),
      CSRRSI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_S),
      CSRRC -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_C),
      CSRRCI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_C),
      ECALL -> List(ALU_X, OP1_X, OP2_X, MEN_X, REN_X, WB_X, CSR_E),
      PCNT -> List(ALU_PCNT, OP1_RS1, OP2_X, MEN_X, REN_S, WB_ALU, CSR_X)
    )
  )
  val exe_fun :: op1_sel :: op2_sel :: mem_wen :: rf_wen :: wb_sel :: csr_cmd :: Nil =
    csignals

  val op1_data = MuxLookup(op1_sel, 0.U(WORD_LEN.W))(
    Seq(
      OP1_RS1 -> rs1_data,
      OP1_PC -> io.if2id.pc,
      OP1_IMZ -> imm_z_uext
    )
  )

  val op2_data = MuxLookup(op2_sel, 0.U(WORD_LEN.W))(
    Seq(
      OP2_RS2 -> rs2_data,
      OP2_IMI -> imm_i_sext,
      OP2_IMS -> imm_s_sext,
      OP2_IMJ -> imm_j_sext,
      OP2_IMU -> imm_u_shifted
    )
  )

  val csr_addr = Mux(csr_cmd === CSR_E, CSR_ADDR_MCAUSE, inst(31, 20))

  reg_rf_wen := rf_wen
  reg_wb_addr := wb_addr

  io.id2if.stall_flg := stall_flg
  io.id2ex.pc := RegNext(io.if2id.pc)
  io.id2ex.inst_id := RegNext(inst_id)
  io.id2ex.wb_addr := RegNext(wb_addr)
  io.id2ex.op1_data := RegNext(op1_data)
  io.id2ex.op2_data := RegNext(op2_data)
  io.id2ex.rs1_data := RegNext(rs1_data)
  io.id2ex.rs2_data := RegNext(rs2_data)
  io.id2ex.imm_i_sext := RegNext(imm_i_sext)
  io.id2ex.imm_s_sext := RegNext(imm_s_sext)
  io.id2ex.imm_b_sext := RegNext(imm_b_sext)
  io.id2ex.imm_u_shifted := RegNext(imm_u_shifted)
  io.id2ex.imm_z_uext := RegNext(imm_z_uext)
  io.id2ex.exe_fun := RegNext(exe_fun)
  io.id2ex.mem_wen := RegNext(mem_wen)
  io.id2ex.csr_cmd := RegNext(csr_cmd)
  io.id2ex.csr_addr := RegNext(csr_addr)
  io.id2ex.rf_wen := RegNext(rf_wen)
  io.id2ex.wb_sel := RegNext(wb_sel)
  io.id2ex.wb_addr := RegNext(wb_addr)
}
