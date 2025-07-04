package common

import chisel3._
import chisel3.util.BitPat

object ExFunc extends ChiselEnum {
  val X, ADD, SUB, AND, OR, XOR, SLL, SRL, SRA, SLT, SLTU, BEQ, BNE, BLT, BGE,
      BLTU, BGEU, JALR, COPY1, MUL, MULH, MULHU, MULHSU, DIV, DIVU, REM, REMU,
      MRET, INVALID =
    Value
}

object Op1Sel extends ChiselEnum {
  val X, RS1, PC, IMZ = Value
}

object Op2Sel extends ChiselEnum {
  val X, RS2, IMI, IMS, IMJ, IMU = Value
}

object RenSel extends ChiselEnum {
  val X, S = Value
}

object WbSel extends ChiselEnum {
  val X, ALU, MEMB, MEMH, MEMW, MEMBU, MEMHU, PC, CSR, SC = Value
}

object MwSel extends ChiselEnum {
  val X, W, H, B, HU, BU = Value
}

object CsrCmd extends ChiselEnum {
  val X, W, S, C = Value
}

object AmoSel extends ChiselEnum {
  val X, ADD, SWAP, LR, SC, XOR, OR, AND = Value
}

object CsrPriv extends ChiselEnum {
  val U = Value(0.U(2.W))
  val M = Value(3.U(2.W))
}

object Consts {
  val WORD_LEN = 32
  val DWORD_LEN = 64
  val START_ADDR = "x_80000000".U(WORD_LEN.W)

  val INST_LEN = 4.U(WORD_LEN.W)
  val BUBBLE = "x_00000013".U(WORD_LEN.W) // [ADDI x0,x0,0] = BUBBLE
  val UNIMP = "x_c0001073".U(WORD_LEN.W) // [CSRRW x0, cycle, x0]

  val REG_ADDR_LEN = 5
  val REG_NUM = 1 << REG_ADDR_LEN

  val CSR_ADDR_LEN = 12
  val CSR_NUM = 1 << CSR_ADDR_LEN

  val EXC_BREAKPOINT = 3.U(WORD_LEN.W)
  val EXC_ECALL_U = 8.U(WORD_LEN.W)
  val EXC_ECALL_M = 11.U(WORD_LEN.W)
  val INT_TIMER = "x_80000007".U(WORD_LEN.W)
}
