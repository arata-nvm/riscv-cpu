package core.pipeline

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._
import core.ImemPortIo
import core.RegFileReaderIo
import common._
import core.MenSel

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
  val exe_fun = Output(ExFunc())
  val mem_wen = Output(MenSel())
  val csr_cmd = Output(CsrCmd())
  val csr_addr = Output(UInt(CSR_ADDR_LEN.W))
  val rf_wen = Output(RenSel())
  val wb_sel = Output(WbSel())
  val wb_addr = Output(UInt(REG_ADDR_LEN.W))
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

  val reg_rf_wen = RegInit(RenSel.X)
  val reg_wb_addr = RegInit(0.U(REG_ADDR_LEN.W))

  val rs1_addr_b = io.if2id.inst(19, 15)
  val rs2_addr_b = io.if2id.inst(24, 20)
  val rs1_data_hazard =
    reg_rf_wen === RenSel.S && rs1_addr_b =/= 0.U && rs1_addr_b === reg_wb_addr
  val rs2_data_hazard =
    reg_rf_wen === RenSel.S && rs2_addr_b =/= 0.U && rs2_addr_b === reg_wb_addr
  val stall_flg = rs1_data_hazard || rs2_data_hazard

  val inst = Mux(
    io.ex2id.branch_taken || stall_flg,
    BUBBLE,
    io.if2id.inst
  )
  val inst_id = Mux(
    io.ex2id.branch_taken || stall_flg,
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
        (rs1_addr === io.me2id.wb_addr && io.me2id.rf_wen === RenSel.S) -> io.me2id.wb_data,
        (rs1_addr === io.wb2id.wb_addr && io.wb2id.rf_wen === RenSel.S) -> io.wb2id.wb_data
      )
    )
  io.regfile_rs2.addr := rs2_addr
  val rs2_data =
    MuxCase(
      io.regfile_rs2.data,
      Seq(
        (rs2_addr === 0.U) -> 0.U(WORD_LEN.W),
        (rs2_addr === io.me2id.wb_addr && io.me2id.rf_wen === RenSel.S) -> io.me2id.wb_data,
        (rs2_addr === io.wb2id.wb_addr && io.wb2id.rf_wen === RenSel.S) -> io.wb2id.wb_data
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

  // format: off
  val csignals = ListLookup(
    inst,
    List(ExFunc.X, Op1Sel.X, Op2Sel.X, MenSel.X, RenSel.X, WbSel.X, CsrCmd.X),
    Array(
      LB ->     List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.MEMB,  CsrCmd.X ),
      LH ->     List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.MEMH,  CsrCmd.X ),
      LW ->     List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.MEMW,  CsrCmd.X ),
      LBU ->    List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.MEMBU, CsrCmd.X ),
      LHU ->    List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.MEMHU, CsrCmd.X ),
      SB ->     List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.IMS, MenSel.B, RenSel.X, WbSel.X,     CsrCmd.X ),
      SH ->     List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.IMS, MenSel.H, RenSel.X, WbSel.X,     CsrCmd.X ),
      SW ->     List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.IMS, MenSel.W, RenSel.X, WbSel.X,     CsrCmd.X ),
      ADD ->    List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      ADDI ->   List( ExFunc.ADD,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SUB ->    List( ExFunc.SUB,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      AND ->    List( ExFunc.AND,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      OR ->     List( ExFunc.OR,     Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      XOR ->    List( ExFunc.XOR,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      ANDI ->   List( ExFunc.AND,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      ORI ->    List( ExFunc.OR,     Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      XORI ->   List( ExFunc.XOR,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SLL ->    List( ExFunc.SLL,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SRL ->    List( ExFunc.SRL,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SRA ->    List( ExFunc.SRA,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SLLI ->   List( ExFunc.SLL,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SRLI ->   List( ExFunc.SRL,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SRAI ->   List( ExFunc.SRA,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SLT ->    List( ExFunc.SLT,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SLTU ->   List( ExFunc.SLTU,   Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SLTI ->   List( ExFunc.SLT,    Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      SLTIU ->  List( ExFunc.SLTU,   Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      BEQ ->    List( ExFunc.BEQ,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.X, WbSel.X,     CsrCmd.X ),
      BNE ->    List( ExFunc.BNE,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.X, WbSel.X,     CsrCmd.X ),
      BGE ->    List( ExFunc.BGE,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.X, WbSel.X,     CsrCmd.X ),
      BGEU ->   List( ExFunc.BGEU,   Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.X, WbSel.X,     CsrCmd.X ),
      BLT ->    List( ExFunc.BLT,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.X, WbSel.X,     CsrCmd.X ),
      BLTU ->   List( ExFunc.BLTU,   Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.X, WbSel.X,     CsrCmd.X ),
      JAL ->    List( ExFunc.ADD,    Op1Sel.PC,  Op2Sel.IMJ, MenSel.X, RenSel.S, WbSel.PC,    CsrCmd.X ),
      JALR ->   List( ExFunc.JALR,   Op1Sel.RS1, Op2Sel.IMI, MenSel.X, RenSel.S, WbSel.PC,    CsrCmd.X ),
      LUI ->    List( ExFunc.ADD,    Op1Sel.X,   Op2Sel.IMU, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      AUIPC ->  List( ExFunc.ADD,    Op1Sel.PC,  Op2Sel.IMU, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      CSRRW ->  List( ExFunc.COPY1,  Op1Sel.RS1, Op2Sel.X,   MenSel.X, RenSel.S, WbSel.CSR,   CsrCmd.W ),
      CSRRWI -> List( ExFunc.COPY1,  Op1Sel.IMZ, Op2Sel.X,   MenSel.X, RenSel.S, WbSel.CSR,   CsrCmd.W ),
      CSRRS ->  List( ExFunc.COPY1,  Op1Sel.RS1, Op2Sel.X,   MenSel.X, RenSel.S, WbSel.CSR,   CsrCmd.S ),
      CSRRSI -> List( ExFunc.COPY1,  Op1Sel.IMZ, Op2Sel.X,   MenSel.X, RenSel.S, WbSel.CSR,   CsrCmd.S ),
      CSRRC ->  List( ExFunc.COPY1,  Op1Sel.RS1, Op2Sel.X,   MenSel.X, RenSel.S, WbSel.CSR,   CsrCmd.C ),
      CSRRCI -> List( ExFunc.COPY1,  Op1Sel.IMZ, Op2Sel.X,   MenSel.X, RenSel.S, WbSel.CSR,   CsrCmd.C ),
      ECALL ->  List( ExFunc.ECALL,  Op1Sel.X,   Op2Sel.X,   MenSel.X, RenSel.X, WbSel.X,     CsrCmd.E ),
      PCNT ->   List( ExFunc.PCNT,   Op1Sel.RS1, Op2Sel.X,   MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      MUL ->    List( ExFunc.MUL,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      MULH ->   List( ExFunc.MULH,   Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      MULHU ->  List( ExFunc.MULHU,  Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      MULHSU -> List( ExFunc.MULHSU, Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      DIV ->    List( ExFunc.DIV,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      DIVU ->   List( ExFunc.DIVU,   Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      REM ->    List( ExFunc.REM,    Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X ),
      REMU ->   List( ExFunc.REMU,   Op1Sel.RS1, Op2Sel.RS2, MenSel.X, RenSel.S, WbSel.ALU,   CsrCmd.X )
    )
  )
  val exe_fun :: op1_sel :: op2_sel :: mem_wen :: rf_wen :: wb_sel :: csr_cmd :: Nil =
    csignals
  // format: on

  val op1_data = MuxLookup(op1_sel, 0.U(WORD_LEN.W))(
    Seq(
      Op1Sel.RS1 -> rs1_data,
      Op1Sel.PC -> io.if2id.pc,
      Op1Sel.IMZ -> imm_z_uext
    )
  )

  val op2_data = MuxLookup(op2_sel, 0.U(WORD_LEN.W))(
    Seq(
      Op2Sel.RS2 -> rs2_data,
      Op2Sel.IMI -> imm_i_sext,
      Op2Sel.IMS -> imm_s_sext,
      Op2Sel.IMJ -> imm_j_sext,
      Op2Sel.IMU -> imm_u_shifted
    )
  )

  val csr_addr = Mux(csr_cmd === CsrCmd.E, CSR_ADDR_MCAUSE, inst(31, 20))

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
