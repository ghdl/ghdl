#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
elab_simulate repro1

clean

echo "Test successful"

exit

export GHDL_STD_FLAGS="--std=08 --work=osvvm"

analyze OSVVM/OsvvmScriptSettingsPkg.vhd
analyze OSVVM/TextUtilPkg.vhd
analyze OSVVM/ResolutionPkg.vhd
analyze OSVVM/NamePkg.vhd
analyze OSVVM/OsvvmGlobalPkg.vhd
analyze OSVVM/VendorCovApiPkg.vhd
analyze OSVVM/TranscriptPkg.vhd
analyze OSVVM/AlertLogPkg.vhd
analyze OSVVM/NameStorePkg.vhd
analyze OSVVM/MessageListPkg.vhd
analyze OSVVM/SortListPkg_int.vhd
analyze OSVVM/RandomBasePkg.vhd
analyze OSVVM/RandomPkg.vhd
analyze OSVVM/RandomProcedurePkg.vhd
analyze OSVVM/CoveragePkg.vhd
# analyze CoveragePkg_new.vhd
analyze OSVVM/ResizePkg.vhd
analyze OSVVM/ScoreboardGenericPkg.vhd
analyze OSVVM/ScoreboardPkg_slv.vhd
analyze OSVVM/ScoreboardPkg_int.vhd
analyze OSVVM/MemorySupportPkg.vhd
analyze OSVVM/MemoryGenericPkg.vhd
analyze OSVVM/MemoryPkg.vhd
analyze OSVVM/TbUtilPkg.vhd
analyze OSVVM/ReportPkg.vhd
analyze OSVVM/OsvvmTypesPkg.vhd
analyze OSVVM/OsvvmContext.vhd

export GHDL_STD_FLAGS="--std=08 --work=osvvm_common"

analyze OSVVM-Common/src/ModelParametersPkg.vhd
analyze OSVVM-Common/src/FifoFillPkg_slv.vhd
analyze OSVVM-Common/src/StreamTransactionPkg.vhd
analyze OSVVM-Common/src/StreamTransactionArrayPkg.vhd
analyze OSVVM-Common/src/AddressBusTransactionPkg.vhd
analyze OSVVM-Common/src/AddressBusTransactionArrayPkg.vhd
analyze OSVVM-Common/src/AddressBusResponderTransactionPkg.vhd
analyze OSVVM-Common/src/AddressBusResponderTransactionArrayPkg.vhd
analyze OSVVM-Common/src/AddressBusVersionCompatibilityPkg.vhd
analyze OSVVM-Common/src/InterruptGlobalSignalPkg.vhd
analyze OSVVM-Common/src/InterruptHandler.vhd
analyze OSVVM-Common/src/InterruptHandlerComponentPkg.vhd
analyze OSVVM-Common/src/InterruptGeneratorBit.vhd
analyze OSVVM-Common/src/InterruptGeneratorBitVti.vhd
# analyzOSVVM-Common/src/e InterruptGenerator.vhd
# analyzOSVVM-Common/src/e InterruptGeneratorVti.vhd
analyze OSVVM-Common/src/InterruptGeneratorComponentPkg.vhd
analyze OSVVM-Common/src/OsvvmCommonContext.vhd

export GHDL_STD_FLAGS="--std=08 --work=osvvm_axi4"

analyze AXI4/common/src/Axi4InterfaceCommonPkg.vhd
analyze AXI4/common/src/Axi4LiteInterfacePkg.vhd
analyze AXI4/common/src/Axi4InterfacePkg.vhd
analyze AXI4/common/src/Axi4CommonPkg.vhd
analyze AXI4/common/src/Axi4ModelPkg.vhd
analyze AXI4/common/src/Axi4OptionsPkg.vhd
analyze AXI4/common/src/Axi4OptionsArrayPkg.vhd
analyze AXI4/common/src/Axi4VersionCompatibilityPkg.vhd

analyze AXI4/Axi4/src/Axi4ComponentPkg.vhd
analyze AXI4/Axi4/src/Axi4ComponentVtiPkg.vhd
analyze AXI4/Axi4/src/Axi4Context.vhd
analyze AXI4/Axi4/src/Axi4Manager.vhd
analyze AXI4/Axi4/src/Axi4ManagerVti.vhd
analyze AXI4/Axi4/src/Axi4Monitor_dummy.vhd
analyze AXI4/Axi4/src/Axi4Subordinate.vhd
analyze AXI4/Axi4/src/Axi4SubordinateVti.vhd
analyze AXI4/Axi4/src/Axi4Memory.vhd
analyze AXI4/Axi4/src/Axi4MemoryVti.vhd
analyze AXI4/Axi4/src/Axi4PassThru.vhd

export GHDL_STD_FLAGS="--std=08"

analyze TestCtrl_e.vhd
analyze TbAddressBusMemory.vhd

analyze TbAb_Interrupt3.vhd

elab_simulate TbAb_Interrupt3 -gNUM_INTERRUPTS=2 --stop-time=10us
clean

echo "Test successful"
