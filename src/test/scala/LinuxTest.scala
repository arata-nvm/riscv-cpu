package core

import chiseltest._
import chiseltest.iotesters.PeekPokeTester
import chiseltest.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import java.io.PrintWriter

class LinuxTest extends AnyFlatSpec with ChiselScalatestTester {
  it must "work through linux" in {
    test(new Top("src/hex/linux.hex", true))
      .withAnnotations(Seq(VerilatorBackendAnnotation))
      .runPeekPoke { c =>
        new PeekPokeTester(c) {
          reset()
          step(1)

          while (true) {
            step(1)
          }
        }
      }
  }
}
