#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="--std=08 -frelaxed"

analyze repro1.vhdl
elab_simulate repro1

analyze repro3.vhdl
elab_simulate repro3

analyze repro2.vhdl
elab_simulate repro2

analyze tb3.vhdl
elab_simulate tb3

analyze repro4.vhdl
elab_simulate repro4

analyze mempkgreproducer4.vhdl
elab_simulate mempkgreproducer4

if false; then
    export GHDL_STD_FLAGS="--std=08 -frelaxed --work=osvvm -Wno-hide"

    analyze OsvvmLibraries/osvvm/TextUtilPkg.vhd
    analyze OsvvmLibraries/osvvm/ResolutionPkg.vhd
    analyze OsvvmLibraries/osvvm/NamePkg.vhd
    analyze OsvvmLibraries/osvvm/OsvvmGlobalPkg.vhd
    analyze OsvvmLibraries/osvvm/VendorCovApiPkg.vhd
    analyze OsvvmLibraries/osvvm/TranscriptPkg.vhd
    analyze OsvvmLibraries/osvvm/AlertLogPkg.vhd
    analyze OsvvmLibraries/osvvm/NameStorePkg.vhd
    #analyze OsvvmLibraries/osvvm/MessageListPkg.vhd
    #analyze OsvvmLibraries/osvvm/SortListPkg_int.vhd
    #analyze OsvvmLibraries/osvvm/RandomBasePkg.vhd
    #analyze OsvvmLibraries/osvvm/RandomPkg.vhd
    #analyze OsvvmLibraries/osvvm/RandomProcedurePkg.vhd
    #analyze OsvvmLibraries/osvvm/CoveragePkg.vhd
    #analyze OsvvmLibraries/osvvm/ScoreboardGenericPkg.vhd
    #analyze OsvvmLibraries/osvvm/ScoreboardPkg_slv.vhd
    #analyze OsvvmLibraries/osvvm/ScoreboardPkg_int.vhd
    #analyze OsvvmLibraries/osvvm/ResizePkg.vhd
    analyze OsvvmLibraries/osvvm/MemorySupportPkg.vhd
    analyze OsvvmLibraries/osvvm/MemoryGenericPkg.vhd
    analyze OsvvmLibraries/osvvm/MemoryPkg.vhd
    #analyze OsvvmLibraries/osvvm/TbUtilPkg.vhd
    #analyze OsvvmLibraries/osvvm/ReportPkg.vhd
    #analyze OsvvmLibraries/osvvm/OsvvmTypesPkg.vhd
    #analyze OsvvmLibraries/osvvm/OsvvmContext.vhd


    export GHDL_STD_FLAGS="--std=08 -frelaxed"

    analyze tb2.vhdl
    elab_simulate tb2

    analyze tb.vhdl
    elab_simulate tb
fi

clean

echo "Test successful"
