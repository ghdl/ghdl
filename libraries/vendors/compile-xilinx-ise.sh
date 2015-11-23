#! /bin/bash

source config.sh

DestDir="$XilinxDestinationDirectory"
SourceDir="$XilinxISEDirectory/ISE_DS/ISE/vhdl/src"
echo $SourceDir

mkdir $DestDir
cd $DestDir

echo "Compiling library 'unisim' ..."

Files=(
	$SourceDir/unisims/unisim_VPKG.vhd
	$SourceDir/unisims/unisim_VCOMP.vhd
)

for File in ${Files[@]}; do
	echo "Analysing package '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unisim $File
done

Files=$SourceDir/unisims/primitive/*.vhd
for File in $Files; do
	echo "Analysing package '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unisim $File
done





cd ..