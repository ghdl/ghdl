#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="--std=08"

analyze repro1.vhdl
elab_simulate repro1

analyze repro2.vhdl
elab_simulate repro2

clean

if false; then
    export GHDL_STD_FLAGS="--std=08 --work=osvvm"

    analyze OSVVM/NamePkg.vhd
    analyze OSVVM/TextUtilPkg.vhd
    analyze OSVVM/OsvvmGlobalPkg.vhd
    analyze OSVVM/OsvvmScriptSettingsPkg.vhd
    analyze OSVVM/TranscriptPkg.vhd
    analyze OSVVM/AlertLogPkg.vhd
    analyze OSVVM/ResolutionPkg.vhd
    analyze OSVVM/NameStorePkg.vhd
    analyze OSVVM/ScoreboardGenericPkg.vhd

    export GHDL_STD_FLAGS="--std=08"

    analyze pkg_a.vhdl
    analyze pkg_b.vhdl
    analyze atest_tb.vhdl
    elab_simulate atest_tb

    clean
    clean osvvm
fi

echo "Test successful"
