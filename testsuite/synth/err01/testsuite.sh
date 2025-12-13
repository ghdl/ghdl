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

synth_failure err_conv01.vhdl -e
synth_failure err_op02.vhdl -e

synth_failure err_resize01.vhdl -e
synth_failure err_resize02.vhdl -e

echo "Test successful"
