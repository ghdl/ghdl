import cocotb
from cocotb.triggers import Timer
from cocotb.binary import BinaryValue

@cocotb.test()
async def test_deposit_bug(dut):
    dut.oe.value = 0

    # Test HighZ
    await Timer(1, 'ns')
    assert dut.o.value == BinaryValue('Z'*8)

    # Test Transparent latch disabled
    dut.d.value = 0
    await Timer(1, 'ns')
    assert dut.o.value == BinaryValue('Z'*8)

    dut.oe.value = 1
    await Timer(1, 'ns')
    assert dut.o.value == 0

    for i in range(64):
        dut.d.value = i
        await Timer(1, 'ns')
        assert dut.o.value == i

    dut.oe.value = 0
    await Timer(1, 'ns')
    assert dut.o.value == BinaryValue('Z'*8)

    #************** Failure happens now **********************
    dut.o.value = 0xFF # This is a deposit for Cocotb
    await Timer(1, 'ns')

    # From this point, the deposit is ignored and Cocotb has full control over the signal o
    dut.d.value = 1
    dut.oe.value = 1
    await Timer(1, 'ns')
    assert dut.o.value == 1

    for i in range(64):
        dut.d.value = i
        await Timer(1, 'ns')
        assert dut.o.value == i
