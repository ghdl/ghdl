import cocotb
from cocotb.triggers import Timer


@cocotb.test()
def test_transaction(dut):

    yield Timer(1)

    dut._log.info("%d" % dut.s_test)
    dut._log.info("%d" % dut.cmp_bar.s_test)
