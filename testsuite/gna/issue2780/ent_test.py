import cocotb

@cocotb.test()
async def ent_test(dut):
    dut.aaa.value = 1
