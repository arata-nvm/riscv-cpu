package core

import chiseltest._
import chiseltest.iotesters.PeekPokeTester
import chiseltest.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import java.io.PrintWriter

class LinuxTest extends AnyFlatSpec with ChiselScalatestTester {
  it must "work through linux" in {
    val writer = new PrintWriter("exec.log")

    test(new Top("src/hex/linux.hex", true))
      .withAnnotations(Seq(VerilatorBackendAnnotation))
      .runPeekPoke { c =>
        new PeekPokeTester(c) {
          reset()
          var prev_pc = 0

          val watchTargets: Seq[Int] = Seq(
            0x801fad98,
            0x801fad6c,
            0x801fadb8,
            0x80002024
          )

          try {
            while (true) {
              step(1024)
              val pc = peek(c.io.pc).toInt
              val inst = peek(c.io.inst).toInt
              val pc_valid = peek(c.io.pc_valid).toInt

              val isWatched = watchTargets.contains(pc) && prev_pc != pc - 4
              val needsPrint =
                pc_valid != 0 && (inst != 0x00000013 || isWatched)

              if (needsPrint) {
                // writer.printf("pc: %08x\n", pc)
                // writer.flush()
              }

              prev_pc = pc
            }
          } finally {
            writer.close() // シミュレーションが終了したらログファイルを閉じる
          }
        }
      }
  }
}
