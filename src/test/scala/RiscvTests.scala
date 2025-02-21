package core

import chisel3._
import org.scalatest._
import chiseltest._

class RiscvTests extends FlatSpec with ChiselScalatestTester {

  val hexDir = "src/hex/riscv-tests/"

  val memoryFiles = Seq(
    "rv32ui-p-sw.hex",
    "rv32ui-p-lw.hex",
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
    "rv32mi-p-scall.hex"
  )

  def runRiscvTests(memoryFile: String) = {
    val memoryFilePath = hexDir + memoryFile

    it must f"pass $memoryFile" in {
      test(new Top(memoryFilePath)) { c =>
        while (!c.io.exit.peek().litToBoolean) {
          c.clock.step(1)
        }
        c.io.gp.expect(1.U)
      }
    }
  }

  behavior of "mycpu"
  for (memoryFile <- memoryFiles) {
    runRiscvTests(memoryFile)
  }
}
