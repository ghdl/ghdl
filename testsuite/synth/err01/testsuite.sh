#! /bin/sh

. ../../testenv.sh

synth_failure err01.vhdl -e
synth_failure err02.vhdl -e
synth_failure err_slv1.vhdl -e
synth_failure --std=08 err_slv2.vhdl -e
synth_failure err_vect1.vhdl -e

synth_failure --std=93 err_elab01.vhdl -e
synth -frelaxed err_elab01.vhdl -e > syn_elab01.vhdl

synth_failure --std=93 err_elab02.vhdl -e
synth -frelaxed err_elab02.vhdl -e > syn_elab02.vhdl

synth_failure --std=93 err_elab03.vhdl -e
synth -frelaxed err_elab03.vhdl -e > syn_elab03.vhdl

synth_failure --std=93 err_elab04.vhdl -e
synth -frelaxed err_elab04.vhdl -e > syn_elab04.vhdl

synth_only case01

synth_failure err_idx01.vhdl -e
synth_failure err_idx02.vhdl -e
synth_failure err_idx03.vhdl -e

synth_failure err_slice01.vhdl -e

synth_failure err_acc01.vhdl -e
synth_failure err_lit01.vhdl -e

synth_failure --std=08 err_concat01.vhdl -e
synth_failure --std=08 err_concat02.vhdl -e
synth_failure --std=08 err_concat03.vhdl -e
synth_failure --std=08 err_concat04.vhdl -e
synth_failure --std=08 err_concat05.vhdl -e
synth_failure --std=08 err_concat06.vhdl -e
synth_failure --std=08 err_concat07.vhdl -e
synth_failure --std=08 err_concat08.vhdl -e

synth_failure err_conv01.vhdl -e
synth_failure err_op02.vhdl -e
synth_failure err_assert01.vhdl -e
synth_failure err_assert02.vhdl -e

synth_failure err_resize01.vhdl -e
synth_failure err_resize02.vhdl -e

synth_failure --std=08 err_tostr01.vhdl -e

synth_failure err_flt01.vhdl -e
synth_failure err_phy01.vhdl -e

synth_failure err_process01.vhdl -e
synth_failure err_process02.vhdl -e
synth_failure err_process03.vhdl -e
synth_failure err_process04.vhdl -e
synth_failure err_process05.vhdl -e

synth_failure foreign01.vhdl -e

GHDL_STD_FLAGS=--std=08
synth_tb eq01

echo "Test successful"
