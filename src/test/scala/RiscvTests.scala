package core

import chisel3._
import org.scalatest._
import chisel3.simulator.EphemeralSimulator._

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.iotesters.PeekPokeTester

class RiscvTests extends AnyFlatSpec with ChiselScalatestTester {
  val hexDir = "src/hex/riscv-tests/"

  val memoryFiles = Seq(
    "rv32ui-p-lb.hex",
    "rv32ui-p-lh.hex",
    "rv32ui-p-lw.hex",
    "rv32ui-p-lbu.hex",
    "rv32ui-p-lhu.hex",
    "rv32ui-p-sh.hex",
    "rv32ui-p-sb.hex",
    "rv32ui-p-sw.hex",
    "rv32ui-p-add.hex",
    "rv32ui-p-addi.hex",
    "rv32ui-p-sub.hex",
    "rv32ui-p-and.hex",
    "rv32ui-p-andi.hex",
    "rv32ui-p-or.hex",
    "rv32ui-p-ori.hex",
    "rv32ui-p-xor.hex",
    "rv32ui-p-xori.hex",
    "rv32ui-p-sll.hex",
    "rv32ui-p-srl.hex",
    "rv32ui-p-sra.hex",
    "rv32ui-p-slli.hex",
    "rv32ui-p-srli.hex",
    "rv32ui-p-srai.hex",
    "rv32ui-p-slt.hex",
    "rv32ui-p-sltu.hex",
    "rv32ui-p-slti.hex",
    "rv32ui-p-sltiu.hex",
    "rv32ui-p-beq.hex",
    "rv32ui-p-bne.hex",
    "rv32ui-p-blt.hex",
    "rv32ui-p-bge.hex",
    "rv32ui-p-bltu.hex",
    "rv32ui-p-bgeu.hex",
    "rv32ui-p-jal.hex",
    "rv32ui-p-jalr.hex",
    "rv32ui-p-lui.hex",
    "rv32ui-p-auipc.hex",
    "rv32mi-p-csr.hex",
    "rv32mi-p-scall.hex",
    "rv32um-p-mul.hex",
    "rv32um-p-mulh.hex",
    "rv32um-p-mulhu.hex",
    "rv32um-p-mulhsu.hex",
    "rv32um-p-div.hex",
    "rv32um-p-divu.hex",
    "rv32um-p-rem.hex",
    "rv32um-p-remu.hex"
  )

  def runRiscvTests(memoryFile: String) = {
    val memoryFilePath = hexDir + memoryFile

    it must f"pass $memoryFile" in {
      test(new Top(memoryFilePath, true)).runPeekPoke { c =>
        new PeekPokeTester(c) {
          reset()
          step(1)

          var cycles = 0
          while (peek(c.io.exit) == 0) {
            assert(cycles <= 1000, "timeout")
            step(1)
            cycles += 1
          }
          expect(c.io.gp, 1)
        }
      }
    }
  }

  for (memoryFile <- memoryFiles) {
    runRiscvTests(memoryFile)
  }
}
