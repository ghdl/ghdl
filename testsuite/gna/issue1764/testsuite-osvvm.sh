#! /bin/sh

. ../../testenv.sh

COMMON_FLAGS="--std=08"
COMMON_FLAGS="$COMMON_FLAGS -O -g"
#COMMON_FLAGS="$COMMON_FLAGS --post -Wp,-g"
export GHDL_STD_FLAGS=$COMMON_FLAGS

GHDL_STD_FLAGS="$COMMON_FLAGS --work=osvvm"
DIR=OSVVM
analyze $DIR/NamePkg.vhd
analyze $DIR/OsvvmGlobalPkg.vhd

analyze $DIR/VendorCovApiPkg.vhd

analyze $DIR/TranscriptPkg.vhd
analyze $DIR/TextUtilPkg.vhd
analyze $DIR/AlertLogPkg.vhd

analyze $DIR/MessagePkg.vhd
analyze $DIR/SortListPkg_int.vhd
analyze $DIR/RandomBasePkg.vhd
analyze $DIR/RandomPkg.vhd
analyze $DIR/CoveragePkg.vhd
analyze $DIR/MemoryPkg.vhd

analyze $DIR/ScoreboardGenericPkg.vhd
analyze $DIR/ScoreboardPkg_slv.vhd
analyze $DIR/ScoreboardPkg_int.vhd

analyze $DIR/ResolutionPkg.vhd
analyze $DIR/TbUtilPkg.vhd

analyze $DIR/OsvvmContext.vhd

GHDL_STD_FLAGS="$COMMON_FLAGS --work=osvvm_common"
DIR=OsvvmLibraries/Common
#analyze $DIR/src/StreamTransactionPkg.vhd
analyze $DIR/src/AddressBusTransactionPkg.vhd
analyze $DIR/src/AddressBusResponderTransactionPkg.vhd
#analyze $DIR/src/AddressBusVersionCompatibilityPkg.vhd
analyze $DIR/src/ModelParametersPkg.vhd
#analyze $DIR/src/FifoFillPkg_slv.vhd
#analyze $DIR/src/InterruptHandler.vhd
#analyze $DIR/src/InterruptHandlerComponentPkg.vhd
analyze $DIR/src/OsvvmCommonContext.vhd

GHDL_STD_FLAGS="$COMMON_FLAGS --work=osvvm_axi4"
DIR=OsvvmLibraries/AXI4/common
analyze $DIR/src/Axi4LiteInterfacePkg.vhd
analyze $DIR/src/Axi4InterfacePkg.vhd
analyze $DIR/src/Axi4CommonPkg.vhd
analyze $DIR/src/Axi4ModelPkg.vhd
analyze $DIR/src/Axi4OptionsPkg.vhd
#analyze $DIR/src/Axi4VersionCompatibilityPkg.vhd

DIR=OsvvmLibraries/AXI4/Axi4
analyze $DIR/src/Axi4ComponentPkg.vhd
#analyze $DIR/src/Axi4ComponentVtiPkg.vhd
analyze $DIR/src/Axi4Context.vhd
analyze $DIR/src/Axi4Master.vhd
#analyze $DIR/src/Axi4MasterVti.vhd
analyze $DIR/src/Axi4Monitor_dummy.vhd
analyze $DIR/src/Axi4Responder_Transactor.vhd
#analyze $DIR/src/Axi4ResponderVti_Transactor.vhd
#analyze $DIR/src/Axi4Memory.vhd
#analyze $DIR/src/Axi4MemoryVti.vhd

GHDL_STD_FLAGS="$COMMON_FLAGS"
DIR=ghdl_issues/GHDL_1764_Axi4Master_Sim_Fails/Axi4Testbench_fails/

#analyze --work=osvvm_axi4 $DIR/Axi4Master.vhd
analyze $DIR/TestCtrl_e_ghdl.vhd
analyze $DIR/TbAxi4.vhd
analyze $DIR/TbAxi4Memory.vhd

DIR=ghdl_issues/GHDL_1764_Axi4Master_Sim_Fails/TestCases/
analyze $DIR/TbAxi4_BasicReadWrite.vhd
elab_simulate TbAxi4_BasicReadWrite

analyze $DIR/TbAxi4_MemoryReadWrite1.vhd
elab_simulate TbAxi4_MemoryReadWrite1

clean

echo "Test successful"
