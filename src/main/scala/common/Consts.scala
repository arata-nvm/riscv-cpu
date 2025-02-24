package common

import chisel3._

object ExFunc extends ChiselEnum {
  val X, ADD, SUB, AND, OR, XOR, SLL, SRL, SRA, SLT, SLTU, BEQ, BNE, BLT, BGE,
      BLTU, BGEU, JALR, COPY1, PCNT, MUL, MULH, MULHU, MULHSU, DIV, DIVU, REM,
      REMU =
    Value
}

object Op1Sel extends ChiselEnum {
  val X, RS1, PC, IMZ = Value
}

object Op2Sel extends ChiselEnum {
  val X, RS2, IMI, IMS, IMJ, IMU = Value
}

object MenSel extends ChiselEnum {
  val X, S = Value
}

object RenSel extends ChiselEnum {
  val X, S = Value
}

object WbSel extends ChiselEnum {
  val X, ALU, MEM, PC, CSR = Value
}

object MwSel extends ChiselEnum {
  val X, W, H, B, HU, BU = Value
}

object CsrCmd extends ChiselEnum {
  val X, W, S, C, E, V = Value
}

object Consts {
  val WORD_LEN = 32
  val START_ADDR = 0.U(WORD_LEN.W)
  val BUBBLE = 0x00000013.U(WORD_LEN.W) // [ADDI x0,x0,0] = BUBBLE
  val UNIMP = "x_c0001073".U(WORD_LEN.W) // [CSRRW x0, cycle, x0]
  val ADDR_LEN = 5 // rs1,rs2,wb
  val INST_LEN = 4.U(WORD_LEN.W)
  val REG_ADDR_LEN = 5
  val REG_NUM = 1 << REG_ADDR_LEN
  val CSR_ADDR_LEN = 12
  val CSR_NUM = 1 << CSR_ADDR_LEN
  val VLEN = 128
  val LMUL_LEN = 2
  val SEW_LEN = 11
  val VL_ADDR = 0xc20
  val VTYPE_ADDR = 0xc21

  val CSR_ADDR_MTVEC = 0x305.U(CSR_ADDR_LEN.W)
  val CSR_ADDR_MCAUSE = 0x342.U(CSR_ADDR_LEN.W)
}
