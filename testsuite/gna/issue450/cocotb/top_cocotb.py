import cocotb
from cocotb.clock import Clock
from cocotb.triggers import Timer

@cocotb.test()
def test_gen(dut):
    cocotb.fork(Clock(dut.clk, 1000).start())

    yield Timer(1000)

    for i in dut:       
        print i._log.info("Found something: %s" % i._fullname)
