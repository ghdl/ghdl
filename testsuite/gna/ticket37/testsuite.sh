#! /bin/sh

. ../../testenv.sh

analyze dispgen.vhdl
elab_simulate dispgen

elab_simulate dispgen -gstr=Hello

analyze genint.vhdl
elab_simulate_failure genint

simulate genint -gVAL=9
simulate genint -gVal=-159

simulate_failure genint -gval=200

analyze genchar.vhdl
elab_simulate_failure genchar

simulate genchar -gVAL=ack
simulate genchar -gVAL="'A'"
simulate genchar -gVAL="'z'"
simulate_failure genchar -gVAL="'0'"
simulate_failure genchar -gVAL=A

analyze genbool.vhdl
elab_simulate_failure genbool

simulate genbool -gval=true
simulate genbool -gval=" True"

analyze genlogic.vhdl
elab_simulate_failure genlogic

simulate genlogic -gvaL="'1'"
simulate genlogic -gvaL="'H'"
simulate_failure genlogic -gvaL="'L'"

clean

echo "Test successful"
