import cocotb
from cocotb.triggers import Timer
from cocotb.binary import BinaryValue

@cocotb.test()
async def test_deposit_bug(dut):
    dut.o.value = 0 # This is a deposit for Cocotb
    await Timer(1, 'ns')

    # From this point, the deposit is ignored and Cocotb has full control over the signal o
    dut.d.value = 1
    dut.oe.value = 1
    await Timer(1, 'ns')
    assert dut.o.value == 1
