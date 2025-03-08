package core

import chiseltest._
import chiseltest.iotesters.PeekPokeTester
import chiseltest.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class CheckCpsTest extends AnyFlatSpec with ChiselScalatestTester {
  it must "work through hex" in {
    test(new Top("src/hex/loop.hex", true))
      .withAnnotations(Seq(VerilatorBackendAnnotation))
      .runPeekPoke { c =>
        new PeekPokeTester(c) {
          reset()
          step(1)

          var cycles = 0

          val startTime = System.nanoTime()
          while (cycles < 1000000) {
            step(1)
            cycles += 1
          }
          val endTime = System.nanoTime()

          val elapsedTimeSec = (endTime - startTime) / 1e9 // 秒に変換
          val cyclesPerSecond = cycles / elapsedTimeSec

          Console.println(f"Cycles per second: $cyclesPerSecond%.2f")
        }
      }
  }
}
