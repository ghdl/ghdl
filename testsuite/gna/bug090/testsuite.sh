#! /bin/sh

. ../../testenv.sh

for i in *.vhdl; do
  analyze_failure $i
done

clean

echo "Test successful"
