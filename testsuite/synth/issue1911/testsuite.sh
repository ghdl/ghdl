#! /bin/sh

. ../../testenv.sh

synth --work=neorv32 -gfifo_depth=16 -gfifo_width=8 -gfifo_rsync=true -gfifo_safe=true  neorv32_package.vhd neorv32_fifo.vhd -e > syn_neorv32_fifo.vhd

echo "Test successful"

