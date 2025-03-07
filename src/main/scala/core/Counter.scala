package core

import chisel3._
import chisel3.util._
import common.Consts._
import common.CsrCmd

class CounterIo extends Bundle {
  val counter = Output(UInt(WORD_LEN.W))
}

class Counter extends Module {
  val io = IO(new CounterIo())

  val reg_counter = RegInit(0.U(WORD_LEN.W))
  reg_counter := reg_counter + 1.U
  io.counter := reg_counter
}
