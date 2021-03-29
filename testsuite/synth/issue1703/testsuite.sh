#! /bin/sh

. ../../testenv.sh

synth_analyze blinker
count=$(grep -c _edge syn_blinker.vhdl)
if [ $count -ne 1 ]; then
  echo "edge gate present"
  exit 1
fi

clean

echo "Test successful"
