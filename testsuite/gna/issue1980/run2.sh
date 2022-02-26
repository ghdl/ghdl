#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze --work=osvvm OsvvmLibraries/osvvm/NamePkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/ResolutionPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/OsvvmGlobalPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/TranscriptPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/TextUtilPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/AlertLogPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/NameStorePkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/SortListPkg_int.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/RandomBasePkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/RandomPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/RandomProcedurePkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/MessageListPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/VendorCovApiPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/CoveragePkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/MemoryPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/ResizePkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/TbUtilPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/ScoreboardGenericPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/ScoreboardPkg_slv.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/ScoreboardPkg_int.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/ReportPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/OsvvmTypesPkg.vhd
analyze --work=osvvm OsvvmLibraries/osvvm/OsvvmContext.vhd

analyze --work=osvvm_common OsvvmLibraries/Common/src/ModelParametersPkg.vhd
analyze --work=osvvm_common OsvvmLibraries/Common/src/FifoFillPkg_slv.vhd
analyze --work=osvvm_common OsvvmLibraries/Common/src/StreamTransactionPkg.vhd
analyze --work=osvvm_common OsvvmLibraries/Common/src/AddressBusTransactionPkg.vhd
analyze --work=osvvm_common OsvvmLibraries/Common/src/AddressBusResponderTransactionPkg.vhd
analyze --work=osvvm_common OsvvmLibraries/Common/src/AddressBusVersionCompatibilityPkg.vhd
analyze --work=osvvm_common OsvvmLibraries/Common/src/InterruptHandlerComponentPkg.vhd
analyze --work=osvvm_common OsvvmLibraries/Common/src/OsvvmCommonContext.vhd

analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/common/src/Axi4CommonPkg.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/common/src/Axi4InterfacePkg.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/common/src/Axi4LiteInterfacePkg.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/common/src/Axi4OptionsPkg.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/common/src/Axi4ModelPkg.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/Axi4/src/Axi4ComponentPkg.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/Axi4/src/Axi4ComponentVtiPkg.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/common/src/Axi4VersionCompatibilityPkg.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/Axi4/src/Axi4Context.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/Axi4/src/Axi4Subordinate.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/Axi4/src/Axi4Manager.vhd
analyze --work=osvvm_axi4 OsvvmLibraries/AXI4/Axi4/src/Axi4Monitor_dummy.vhd

analyze --work=osvvm_tbaxi4 OsvvmLibraries/AXI4/Axi4/testbench/TbAxi4.vhd
analyze --work=osvvm_tbaxi4 OsvvmLibraries/AXI4/Axi4/testbench/TestCtrl_e.vhd
analyze --work=osvvm_tbaxi4 OsvvmLibraries/AXI4/Axi4/TestCases/TbAxi4_BasicReadWrite.vhd

elab_simulate --work=osvvm_tbaxi4 TbAxi4_BasicReadWrite
