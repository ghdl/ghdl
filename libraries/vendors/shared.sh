# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	Bash Script:				This is a Bash resource file. 
# 
# Description:
# ------------------------------------
#		TODO
#
# ==============================================================================
#	Copyright (C) 2017-2019 Patrick Lehmann - Boetzingen, Germany
#	Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
#	
#	GHDL is free software; you can redistribute it and/or modify it under
#	the terms of the GNU General Public License as published by the Free
#	Software Foundation; either version 2, or (at your option) any later
#	version.
#	
#	GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#	WARRANTY; without even the implied warranty of MERCHANTABILITY or
#	FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#	for more details.
#	
#	You should have received a copy of the GNU General Public License
#	along with GHDL; see the file COPYING.  If not, write to the Free
#	Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#	02111-1307, USA.
# ==============================================================================


# set bash options
set -o pipefail

SetupDirectories() {
	Index=$1
	Name=$2

	# source directory
	# ----------------------
	# If a command line argument ('--src') was passed in, use it, else use the default value
	# from config.sh
	if [ ! -z "$SrcDir" ]; then
		SourceDirectory=${SrcDir%/}										# remove trailing slashes
	elif [ ! -z "$EnvSourceDir" ]; then
		SourceDirectory=$EnvSourceDir									# fall back to environment variable
	elif [ ! -z "${InstallationDirectories[$Index]}" ]; then
		SourceDirectory=${InstallationDirectories[$Index]}/${SourceDirectories[$Index]}	# fall back to value from config.sh
	fi
	# output directory
	# ----------------------
	# If a command line argument ('--out') was passed in, use it, else use the default value
	# from config.sh
	if [ ! -z "$DestDir" ]; then
		DestinationDirectory=${DestDir%/}												# remove trailing slashes
	else
		DestinationDirectory=${DestinationDirectories[$Index]}	# fall back to value from config.sh
	fi

	if [ -z $SourceDirectory ] || [ -z $DestinationDirectory ]; then
		echo 1>&2 -e "${COLORED_ERROR} $Name is not configured in '$ScriptDir/config.sh'.${ANSI_NOCOLOR}"
		echo 1>&2 -e "  Use adv. options '--src' and '--out' or configure 'config.sh'."
		exit 1
	elif [ ! -d $SourceDirectory ]; then
		echo 1>&2 -e "${COLORED_ERROR} Path '$SourceDirectory' does not exist.${ANSI_NOCOLOR}"
		exit 1
	fi

	# Resolve paths to an absolute paths
	READLINK=readlink; if [[ $(uname) == "Darwin" ]]; then READLINK=greadlink; fi
	SourceDirectory=$($READLINK -f $SourceDirectory)
	if [[ ! "$DestinationDirectory" = /* ]]; then
		DestinationDirectory=$WorkingDir/$DestinationDirectory
	fi

	# Use GHDL binary directory from command line argument, if set
	if [ ! -z "$GHDLBinDir" ]; then
		GHDLBinary=${GHDLBinDir%/}/ghdl		# remove trailing slashes
		if [[ ! -x "$GHDLBinary" ]]; then
			echo 1>&2 -e "${COLORED_ERROR} GHDL not found or is not executable.${ANSI_NOCOLOR}"
			exit 1
		fi
	elif [ ! -z "$GHDL" ]; then
		if [ ! \( -f "$GHDL" -a -x "$GHDL" \) ]; then
			echo 1>&2 -e "${COLORED_ERROR} Found GHDL environment variable, but '$GHDL' is not executable.${ANSI_NOCOLOR}"
			exit 1
		fi
		GHDLBinary=$GHDL
	else	# fall back to GHDL found via PATH
		GHDLBinary=$(which ghdl 2>/dev/null)
		if [ $? -ne 0 ]; then
			echo 1>&2 -e "${COLORED_ERROR} GHDL not found in PATH.${ANSI_NOCOLOR}"
			echo 1>&2 -e "  Use adv. options '--ghdl' to set the GHDL binary directory."
			exit 1
		fi
	fi
}

SetupGRCat() {
	if [ -z "$(which grcat 2>/dev/null)" ]; then
		# if grcat (generic colourizer) is not installed, use a dummy pipe command like 'cat'
		GRC_COMMAND="cat"
	elif [ $SUPPRESS_WARNINGS -eq 1 ]; then
		GRC_COMMAND="grcat $ScriptDir/ghdl.skipwarning.grcrules"
	else
		GRC_COMMAND="grcat $ScriptDir/ghdl.grcrules"
	fi
}

CreateDestinationDirectory() {
	if [ -d "$DestinationDirectory" ]; then
		echo -e "${ANSI_YELLOW}Vendor directory '$DestinationDirectory' already exists.${ANSI_NOCOLOR}"
	elif [ -f "$DestinationDirectory" ]; then
		echo 1>&2 -e "${COLORED_ERROR} Vendor directory '$DestinationDirectory' already exists as a file.${ANSI_NOCOLOR}"
		exit 1
	else
		echo -e "${ANSI_YELLOW}Creating vendor directory: '$DestinationDirectory'.${ANSI_NOCOLOR}"
		mkdir -p "$DestinationDirectory"
	fi
}

GHDLSetup() {
	if [ $VHDLStandard -eq 93 ]; then
		VHDLVersion="v93"
		VHDLStandard="93c"
		VHDLFlavor="synopsys"
	elif [ $VHDLStandard -eq 2008 ]; then
		VHDLVersion="v08"
		VHDLStandard="08"
		VHDLFlavor="synopsys"
	fi
}

GHDLCompileLibrary() {
	# assembling output directory
	LibraryDirectory=$DestinationDirectory/$Library/$VHDLVersion
	mkdir -p $LibraryDirectory
	cd $LibraryDirectory
	echo -e "${ANSI_YELLOW}Compiling library '$Library'...${ANSI_NOCOLOR}"

	for File in ${SourceFiles[@]}; do
		FileName=$(basename "$File")
		FileSize=($(wc -c $File))
		if [ $SKIP_EXISTING_FILES -eq 1 ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping existing file '$File'${ANSI_NOCOLOR}"
		elif [ $SKIP_LARGE_FILES -eq 1 ] && [ ${FileSize[0]} -gt $LARGE_FILESIZE ]; then
			echo -e "${ANSI_CYAN}Skipping large file '$File'${ANSI_NOCOLOR}"
		else
			echo -e "${ANSI_DARKCYAN}Analyzing file '$File'${ANSI_NOCOLOR}"
			$GHDLBinary -a ${GHDL_PARAMS[@]} --work=$Library "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				test $HALT_ON_ERROR -eq 1 && return 1
			fi
		fi
	done
	return 0
}

GHDLCompilePackages() {
	# assembling output directory
	LibraryDirectory=$DestinationDirectory/$Library/$VHDLVersion
	mkdir -p $LibraryDirectory
	cd $LibraryDirectory
	echo -e "${ANSI_YELLOW}Compiling library '$Library'...${ANSI_NOCOLOR}"

	for File in ${SourceFiles[@]}; do
		FileName=$(basename "$File")
		if [ $SKIP_EXISTING_FILES -eq 1 ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping existing package '$File'${ANSI_NOCOLOR}"
		else
			echo -e "${ANSI_DARKCYAN}Analyzing package '$File'${ANSI_NOCOLOR}"
			$GHDLBinary -a ${GHDL_PARAMS[@]} --work=$Library "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				test $HALT_ON_ERROR -eq 1 && return 1
			fi
		fi
	done
	return 0
}


test $VERBOSE -eq 1 && echo -e "  Declaring Bash functions for GHDL..."

GHDL="ghdl"

Analyze_Filter=GHDL/filter.analyze.sh
Analyze_Parameters=(
	--std=08
	-frelaxed-rules
	--mb-comments
)

test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}function CreateVHDLLibrary( <LibraryName> <DirectoryName> )${ANSI_NOCOLOR}"
# CreateVHDLLibrary
# -> $LibraryName
# -> $DirectoryName
CreateVHDLLibrary() {
	local LibraryName=$1
	local DirectoryName=$2

	if [[ $DEBUG -eq 1 ]]; then
		local Filter_Parameters=(
			-d
		)
		local Filter_Indent="      "
	elif [[ $VERBOSE -eq 1 ]]; then
		local Filter_Parameters=(
			-v
		)
		local Filter_Indent="    "
	else
		local Filter_Parameters=()
		local Filter_Indent="  "
	fi

	echo -e "${ANSI_YELLOW}Creating VHDL Library '$LibraryName'...${ANSI_NOCOLOR}"
}

test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}function AnalyzeVHDL( <LibraryName> <File> )${ANSI_NOCOLOR}"
# AnalyzeVHDL
# -> $LibraryName
# -> $File
AnalyzeVHDL() {
	local LibraryName=$1
	local RootDir=$2
	local LibraryPath=$3
	local File=$4

	if [[ $DEBUG -eq 1 ]]; then
		local Parameters=(
			-v
		)
		local Filter_Parameters=(
			-d
		)
		local Filter_Indent="      "
	elif [[ $VERBOSE -eq 1 ]]; then
		local Parameters=()
		local Filter_Parameters=(
			-v
		)
		local Filter_Indent="    "
	else
		local Parameters=()
		local Filter_Parameters=()
		local Filter_Indent="  "
	fi

	echo "spam"
	
	if [[ $FILTERING -eq 0 ]]; then
		test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}$GHDL -a ${Analyze_Parameters[*]} ${Parameters[*]} --work=$LibraryName \"$RootDir/$LibraryPath/$File\"${ANSI_NOCOLOR}"
		$GHDL -a ${Analyze_Parameters[@]} ${Parameters[@]} --work=$LibraryName "$RootDir/$LibraryPath/$File"
		ExitCode=$?
		if [ $ExitCode -ne 0 ]; then
			echo 1>&2 -e "${COLORED_ERROR} While analyzing '$File'. ExitCode: $ExitCode${ANSI_NOCOLOR}"
			exit 1;
		fi
	else
		test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}$GHDL -a ${Analyze_Parameters[*]} ${Parameters[*]} --work=$LibraryName \"$RootDir/$LibraryPath/$File\" | \\\\${ANSI_NOCOLOR}"
		test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}$GHDLScriptDir/$Analyze_Filter ${Filter_Parameters[*]} -i \"$Filter_Indent\"${ANSI_NOCOLOR}"
		$GHDL -a ${Analyze_Parameters[@]} ${Parameters[@]} --work=$LibraryName "$RootDir/$LibraryPath/$File" 2>&1 | $GHDLScriptDir/$Analyze_Filter ${Filter_Parameters[@]} -i "$Filter_Indent"
		local PiplineStatus=("${PIPESTATUS[@]}")
		if [[ ${PiplineStatus[0]}  -ne 0 ]]; then
			echo 1>&2 -e "${COLORED_ERROR} While analyzing '$File'. ExitCode: ${PiplineStatus[0]}${ANSI_NOCOLOR}"
			exit 1;
		elif [[ ${PiplineStatus[1]}  -ne 0 ]]; then
			case $(( ${PiplineStatus[1]} % 4 )) in
				3) echo 1>&2 -e "$Filter_Indent${ANSI_RED}Fatal errors detected by filtering script. ExitCode: ${PiplineStatus[1]}${ANSI_NOCOLOR}"; exit 1 ;;
				2) echo 1>&2 -e "$Filter_Indent${ANSI_RED}Errors detected by filtering script. ExitCode: ${PiplineStatus[1]}${ANSI_NOCOLOR}"; exit 1 ;;
				1) echo 1>&2 -e "$Filter_Indent${ANSI_YELLOW}Warnings detected by filtering script.${ANSI_NOCOLOR}" ;;
				0) test $DEBUG -eq 1 && echo 1>&2 -e "$Filter_Indent${ANSI_YELLOW}Warnings detected by filtering script.${ANSI_NOCOLOR}" ;;
			esac
		fi
	fi
}


test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}function AnalyzeLibrary( <Library> )${ANSI_NOCOLOR}"
# AnalyzeLibrary
# -> LibraryName
# -> LibraryPath
# -> Files[*]
AnalyzeLibrary() {
	local LibraryName=$1; shift
	local LibraryPath=$1; shift
	local Files=$@

	echo -e "${ANSI_YELLOW}Analyzing files into library '$LibraryName'...${ANSI_NOCOLOR}"
	
	for File in $Files; do
		test $VERBOSE -eq 1 && echo -e "  Analyzing '$File'"
	
		AnalyzeVHDL $LibraryName "$RootDir" "$LibraryPath" "$File"
	done
}

test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}function Compile( <Libraries> )${ANSI_NOCOLOR}"
# Compile
# -> VHDLLibraries
Compile() {
	local VHDLLibraries=$1

	for VHDLLibrary in $VHDLLibraries; do
		local LibraryName="${VHDLLibrary}_LibraryName"; LibraryName=${!LibraryName}
		local LibraryPath="${VHDLLibrary}_LibraryPath"; LibraryPath=${!LibraryPath}
		local Files="${VHDLLibrary}_Files[*]";          Files=${!Files}

		echo -e "${ANSI_LIGHT_CYAN}Analyzing library '$LibraryName'...${ANSI_NOCOLOR}"

		CreateVHDLLibrary $LibraryName $LibraryName
		AnalyzeLibrary $LibraryName "$LibraryPath" "$Files"
	done
}
