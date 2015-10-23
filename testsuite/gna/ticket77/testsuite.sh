#! /bin/sh

. ../../testenv.sh

analyze_elab_simulate_status()
{
  file=$1
  ent=$2
  status=$3

  analyze $file
  set +e
  echo "elaborate and simulate $ent"
  "$GHDL" --elab-run $GHDL_STD_FLAGS $ent
  if [ $? -ne $status ]; then
    echo "Exit status $status expected"
    exit 1
  fi
  set -e
}

GHDL_STD_FLAGS=--std=08
analyze_elab_simulate_status bug1.vhdl ent1 1
analyze_elab_simulate_status bug2.vhdl ent2 1
analyze_elab_simulate_status bug3.vhdl ent3 7
analyze_elab_simulate_status bug4.vhdl ent4 0

clean

echo "Test successful"
