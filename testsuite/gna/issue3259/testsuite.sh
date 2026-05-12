#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze tb_finish.vhdl
analyze tb_no_finish.vhdl

elab tb_finish
elab tb_no_finish

# Explicit finish before timeout.  std.env.finish should exit gracefully.
simulate tb_finish --expect-finish --stop-time=10ns

# No std.env.finish should return an error code.
if simulate tb_no_finish --expect-finish --stop-time=1ns; then
    echo "Failure: An error expected on stop time being reached."
    exit 1
fi

# Preservice old functionality; that is a timeout on --stop-time
# without --expect-failure returns a 0 status for success.
simulate tb_no_finish --stop-time=1ns

clean

echo "Test successful"
