#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

for t in repro1 repro2; do
  analyze $t.vhdl
  if ghdl_is_preelaboration; then
    elab_simulate $t
  else
    # External names are not supported by the gcc and llvm backends:
    # expect a clear error instead of a null access or a silent abort.
    if "$GHDL" --elab-run $GHDL_STD_FLAGS $GHDL_FLAGS $t > $t.err 2>&1; then
      echo "failure expected"
      exit 1
    fi
    cat $t.err
    grep -q "external names are not supported by this backend" $t.err
  fi
done

rm -f repro1.err repro2.err
clean

echo "Test successful"
