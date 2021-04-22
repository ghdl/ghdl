#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=" -fexplicit -frelaxed-rules --mb-comments --warn-binding --ieee=synopsys --no-vital-checks --std=08"

analyze ilos_sim_pkg.vhd
analyze irqc_pif_pkg.vhd
analyze irqc_pif.vhd
analyze irqc_tb.vhd
elab tb_irqc

if ghdl_has_feature tb_irqc ghw; then
  simulate tb_irqc --wave=sim.ghw
  ghw_diff sim
  rm -f sim.txt sim.ghw
fi

analyze repro_rec.vhdl
elab repro_rec

if ghdl_has_feature repro_rec ghw; then
  simulate repro_rec --wave=rec.ghw
  ghw_diff rec
  rm -f rec.txt rec.ghw
fi

clean

echo "Test successful"
