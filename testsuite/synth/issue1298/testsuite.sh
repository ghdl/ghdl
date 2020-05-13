#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="-fsynopsys -fexplicit -frelaxed --std=08"

# synth -gBOO=true -gINT=255 -gLOG=\'1\' -gSTR="WXYZ" generics.vhdl -e Params

synth -gBOO=true -gINT=255 -gLOG=\'1\' -gSTR="WXYZ" -gVEC="11111111" --out=none generics.vhdl -e Params

synth_failure -gBOO=true -gINT=255 -gLOG=\'1\' -gSTR="WXYZ" -gVEC=\"11111111\" --out=none generics.vhdl -e Params

synth_failure -gBOO=true -gINT=255 -gLOG=\'1\' -gSTR="WXYZ" -gREA=1.1 generics.vhdl -e Params

echo "Test successful"
