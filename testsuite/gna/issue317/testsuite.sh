#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="--std=08"

# Reproducer1 (declaration of a record in an uninstantiated package)
analyze repro1.vhdl
elab_simulate repro1

# Reproducer2 (package instantiation within an uninstantiated package)
analyze repro2.vhdl
elab_simulate repro2

# Reproducer3 (package instantiation with implicit subprograms)
analyze repro3.vhdl
elab_simulate repro3

analyze repro4.vhdl
elab_simulate repro4

# Reproducer5 (with a protected type)
analyze repro5.vhdl
elab_simulate repro5

# Reproducer6
analyze repro6.vhdl
elab_simulate repro6

# OSVVM
if true; then
analyze --work=osvvm OSVVM/NamePkg.vhd
analyze --work=osvvm OSVVM/OsvvmGlobalPkg.vhd
analyze --work=osvvm OSVVM/TranscriptPkg.vhd
analyze --work=osvvm OSVVM/TextUtilPkg.vhd
analyze --work=osvvm OSVVM/AlertLogPkg.vhd
analyze --work=osvvm OSVVM/RandomBasePkg.vhd
analyze --work=osvvm OSVVM/SortListPkg_int.vhd
analyze --work=osvvm OSVVM/RandomPkg.vhd
#analyze --work=osvvm MessagePkg.vhd
#analyze --work=osvvm VendorCovApiPkg.vhd
#analyze --work=osvvm CoveragePkg.vhd
#analyze --work=osvvm MemoryPkg.vhd
analyze --work=osvvm OSVVM/ScoreboardGenericPkg.vhd
#analyze --work=osvvm ScoreboardPkg_int.vhd
#analyze --work=osvvm ScoreboardPkg_slv.vhd

#analyze --work=osvvm ResolutionPkg.vhd
#analyze --work=osvvm TbUtilPkg.vhd
#analyze --work=osvvm OsvvmContext.vhd
fi

# PoC (Poc)
analyze --work=poc my_config.vhdl
analyze --work=poc my_project.vhdl

analyze --work=poc PoC/src/common/utils.vhdl
analyze --work=poc PoC/src/common/config.vhdl
analyze --work=poc PoC/src/common/math.vhdl
analyze --work=poc PoC/src/common/strings.vhdl
analyze --work=poc PoC/src/common/physical.vhdl
analyze --work=poc PoC/src/common/vectors.vhdl
analyze --work=poc PoC/src/common/protected.v08.vhdl
analyze --work=poc PoC/src/common/fileio.v08.vhdl
analyze --work=poc PoC/src/common/components.vhdl

analyze --work=poc PoC/src/sim/sim_types.vhdl
analyze --work=poc PoC/src/sim/sim_protected.v08.vhdl
analyze --work=poc PoC/src/sim/sim_global.v08.vhdl
analyze --work=poc PoC/src/sim/sim_simulation.v08.vhdl
analyze --work=poc PoC/src/sim/sim_waveform.vhdl

analyze --work=poc PoC/src/sort/sortnet/sortnet_BitonicSort.vhdl

# PoC (test)
analyze --work=test PoC/tb/common/config_tb.vhdl


# Testcase
analyze --work=test PoC/tb/sort/sortnet/sortnet_tb.pkg.vhdl
analyze --work=test PoC/tb/sort/sortnet/sortnet_BitonicSort_tb.vhdl
elab_simulate --work=test sortnet_BitonicSort_tb --ieee-asserts=disable-at-0

clean
clean osvvm
clean poc
clean test


echo "Test successful"
