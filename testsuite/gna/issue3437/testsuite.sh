#! /bin/sh

. ../../testenv.sh

check_ok()
{
    val=$1
    exp=$2
    elab_simulate repro -gv="$val" > res.out
    if ! grep -q "<$exp>" res.out; then
	echo "Error: expect <$exp>, got:"
	cat res.out
	return 1
    fi
}

if ghdl_is_preelaboration; then

    analyze repro.vhdl

    check_ok "10 ps" "10000 fs"
    check_ok "10ps" "10000 fs"
    check_ok "10.2PS" "10200 fs"
    check_ok "fs" "1 fs"
    check_ok "ps" "1000 fs"
    check_ok " -2 ps " "-2000 fs"
    check_ok "-2e2 fs" "-200 fs"
    check_ok "-2.5e+3 fs" "-2500 fs"
    check_ok "1.2e-1 ps" "120 fs"

    elab_simulate_failure repro -gv="1.ps"
    elab_simulate_failure repro -gv="1hour"
    elab_simulate_failure repro -gv="-ps"
    elab_simulate_failure repro -gv="1e 3ps"

    clean
fi


echo "Test successful"
