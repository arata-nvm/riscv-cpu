package core

import chiseltest._
import chiseltest.iotesters.PeekPokeTester
import chiseltest.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class CoremarkTest extends AnyFlatSpec with ChiselScalatestTester {
  it must "work through coremark" in {
    test(new Top("src/hex/coremark.hex", true))
      .withAnnotations(Seq(VerilatorBackendAnnotation))
      .runPeekPoke { c =>
        new PeekPokeTester(c) {
          reset()
          step(1)

          while (
            peek(c.io.pc).toInt != 0x8 || peek(c.io.inst).toInt != 0x0000006f
          ) {
            step(1)
          }
        }
      }
  }
}
