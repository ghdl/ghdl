#!/usr/bin/python2.7
from __future__ import print_function

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import Timer, RisingEdge

@cocotb.test(timeout=None)
def proto(dut):
  CLK_PERIOD = 15

  dut.arg_a <= 0
  dut.arg_b <= 0

  dut_clk = Clock(dut.clk, CLK_PERIOD, 'ns')
  clk = cocotb.fork(dut_clk.start())

  dut.reset <= 1
  for i in range(2):
     yield RisingEdge(dut.clk)
  dut.reset <= 0
  for i in range(2):
     yield RisingEdge(dut.clk)
  yield Timer(1)
  dut.arg_a <= 8
  dut.arg_b <= 6
  for i in range(5):
     yield RisingEdge(dut.clk)
     print('Value in dut:', dut.sub_module.arg.value)
     yield Timer(1)
     dut.arg_a <= 8 + i
     dut.arg_b <= 6 - i
