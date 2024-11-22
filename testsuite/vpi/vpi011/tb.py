import cocotb

@cocotb.test()
async def some_test(dut):
    print(dut.mem.value)
