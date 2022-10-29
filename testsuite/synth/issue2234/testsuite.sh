#! /bin/sh

. ../../testenv.sh

# synth --out=verilog --std=08 --no-formal --work=neorv32 -gCONFIG=2 -gDEBUG=False neorv32_bus_keeper.vhd neorv32_busswitch.vhd neorv32_cpu_alu.vhd neorv32_cpu_bus.vhd neorv32_cpu_control.vhd neorv32_cpu_cp_bitmanip.vhd neorv32_cpu_cp_cfu.vhd neorv32_cpu_cp_fpu.vhd neorv32_cpu_cp_muldiv.vhd neorv32_cpu_cp_shifter.vhd neorv32_cpu_decompressor.vhd neorv32_cpu_regfile.vhd neorv32_cpu.vhd neorv32_debug_dm.vhd neorv32_debug_dtm.vhd neorv32_dmem.default.vhd neorv32_fifo.vhd neorv32_icache.vhd neorv32_imem.default.vhd neorv32_litex_core_complex.vhd neorv32_mtime.vhd neorv32_package.vhd neorv32_sysinfo.vhd neorv32_top.vhd neorv32_wishbone.vhd -e neorv32_litex_core_complex

synth --out=verilog -gicache_num_blocks=8 -gicache_block_size=8 -gicache_num_sets=1 --work=neorv32 neorv32_package.vhd neorv32_icache.vhd -e > icache.v

echo "Test successful"
