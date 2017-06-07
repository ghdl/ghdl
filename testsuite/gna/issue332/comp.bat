ghdl -a -fexplicit -frelaxed-rules --mb-comments --warn-binding --ieee=synopsys --no-vital-checks --std=08 ilos_sim_pkg.vhd
ghdl -a -fexplicit -frelaxed-rules --mb-comments --warn-binding --ieee=synopsys --no-vital-checks --std=08 irqc_pif_pkg.vhd
ghdl -a -fexplicit -frelaxed-rules --mb-comments --warn-binding --ieee=synopsys --no-vital-checks --std=08 irqc_pif.vhd
ghdl -a -fexplicit -frelaxed-rules --mb-comments --warn-binding --ieee=synopsys --no-vital-checks --std=08 irqc_tb.vhd
ghdl -r -fexplicit -frelaxed-rules --mb-comments --warn-binding --ieee=synopsys --no-vital-checks --std=08 tb_irqc --wave=sim.ghw
rem ghdl -r -fexplicit -frelaxed-rules --mb-comments --warn-binding --ieee=synopsys --no-vital-checks --std=08 tb_irqc --vcd=sim.vcd