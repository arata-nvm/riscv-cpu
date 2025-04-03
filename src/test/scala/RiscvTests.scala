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
    // // RV32I
    "rv32ui-p-lb.hex",
    "rv32ui-p-lh.hex",
    "rv32ui-p-lw.hex",
    "rv32ui-p-lbu.hex",
    "rv32ui-p-lhu.hex",
    "rv32ui-p-sb.hex",
    "rv32ui-p-sh.hex",
    "rv32ui-p-sw.hex",
    "rv32ui-p-add.hex",
    "rv32ui-p-addi.hex",
    "rv32ui-p-sub.hex",
    "rv32ui-p-and.hex",
    "rv32ui-p-or.hex",
    "rv32ui-p-xor.hex",
    "rv32ui-p-andi.hex",
    "rv32ui-p-ori.hex",
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
    "rv32ui-p-bge.hex",
    "rv32ui-p-bgeu.hex",
    "rv32ui-p-blt.hex",
    "rv32ui-p-bltu.hex",
    "rv32ui-p-jal.hex",
    "rv32ui-p-jalr.hex",
    "rv32ui-p-lui.hex",
    "rv32ui-p-auipc.hex",
    "rv32mi-p-scall.hex",
    "rv32mi-p-sbreak.hex",
    "rv32mi-p-ma_addr.hex",
    // Zicsr
    "rv32mi-p-csr.hex",
    // Zifencei
    "rv32ui-p-fence_i.hex",
    // M
    "rv32um-p-mul.hex",
    "rv32um-p-mulh.hex",
    "rv32um-p-mulhu.hex",
    "rv32um-p-mulhsu.hex",
    "rv32um-p-div.hex",
    "rv32um-p-divu.hex",
    "rv32um-p-rem.hex",
    "rv32um-p-remu.hex",
    // A
    "rv32ua-p-amoadd_w.hex",
    "rv32ua-p-amoswap_w.hex",
    "rv32ua-p-lrsc.hex",
    "rv32ua-p-amoor_w.hex",
    "rv32ua-p-amoand_w.hex"
    // "rv32ua-p-amomax_w.hex",
    // "rv32ua-p-amomaxu_w.hex",
    // "rv32ua-p-amomin_w.hex",
    // "rv32ua-p-amominu_w.hex",
    // "rv32ua-p-amoxor_w.hex",
  )

  def runRiscvTests(memoryFile: String) = {
    val memoryFilePath = hexDir + memoryFile

    it must f"pass $memoryFile" in {
      test(new Top(memoryFilePath, true))
        .runPeekPoke { c =>
          new PeekPokeTester(c) {
            reset()
            step(1)

            var cycles = 0

            while ((peek(c.io.pc).toInt & 0x3ffffff) != 0x44) {
              assert(cycles <= 100000, "timeout")
              step(1)
              cycles += 1
              // Console.printf("pc: %08x\n", peek(c.io.pc).toInt)
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
