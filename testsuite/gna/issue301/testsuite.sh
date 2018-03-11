#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

analyze --work=dec_viterbi packages/pkg_helper.vhd
analyze --work=dec_viterbi packages/pkg_param.vhd
analyze --work=dec_viterbi packages/pkg_param_derived.vhd
analyze --work=dec_viterbi packages/pkg_types.vhd
analyze --work=dec_viterbi packages/pkg_components.vhd
analyze --work=dec_viterbi packages/pkg_trellis.vhd
analyze --work=dec_viterbi src/generic_sp_ram.vhd
analyze --work=dec_viterbi src/axi4s_buffer.vhd
analyze --work=dec_viterbi src/branch_distance.vhd
analyze --work=dec_viterbi src/traceback.vhd
analyze --work=dec_viterbi src/acs.vhd
analyze --work=dec_viterbi src/ram_ctrl.vhd
analyze --work=dec_viterbi src/reorder.vhd
analyze --work=dec_viterbi src/recursion.vhd
analyze --work=dec_viterbi src/dec_viterbi.vhd

elab_simulate --work=dec_viterbi dec_viterbi_top

clean
clean dec_viterbi

echo "Test successful"
