package core

import chiseltest._
import chiseltest.iotesters.PeekPokeTester
import chiseltest.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class HexTest extends AnyFlatSpec with ChiselScalatestTester {
  it must "work through hex" in {
    test(new Top("src/hex/pcnt.hex", false))
      .runPeekPoke { c =>
        new PeekPokeTester(c) {
          reset()
          step(1)

          var cycles = 0
          while (peek(c.io.exit) == 0) {
            assert(cycles <= 1000, "timeout")
            step(1)
            cycles += 1
          }
        }
      }
  }
}
