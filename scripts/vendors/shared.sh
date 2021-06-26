# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
#	 Bash Script:
#	   This is a Bash resource file for sourcing.
#
#  Description:
#	   Provide Bash procedures to easily use GHDL in Bash scripts.
#
# ==============================================================================
#  Copyright (C) 2017-2021 Patrick Lehmann - Boetzingen, Germany
#  Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
# ==============================================================================


# set bash options
set -o pipefail

if [[ -n "$GHDL" ]]; then
	if [[ ! -f "$GHDL" ]]; then
		echo 1>&2 -e "${COLORED_ERROR} Found GHDL environment variable, but '$GHDL' is not a file.${ANSI_NOCOLOR}"
		exit 1
	elif [[ ! -x "$GHDL" ]]; then
		echo 1>&2 -e "${COLORED_ERROR} Found GHDL environment variable, but '$GHDL' is not executable.${ANSI_NOCOLOR}"
		exit 1
	fi
else	# fall back to GHDL found via PATH
	GHDL=$(which ghdl 2>/dev/null)
	if [[ $? -ne 0 ]]; then
		echo 1>&2 -e "${COLORED_ERROR} GHDL not found in PATH.${ANSI_NOCOLOR}"
		echo 1>&2 -e "  Use adv. options '--ghdl' to set the GHDL binary directory."
		exit 1
	fi
fi

Analyze_Filter=filter.analyze.sh
Analyze_Parameters=(
	--mb-comments
)

VERBOSE=${VERBOSE:-0}
DEBUG=${DEBUG:-0}
CONTINUE_ON_ERROR=${CONTINUE_ON_ERROR:-0}

test "$VERBOSE" -eq 1 && echo -e "  Declaring Bash procedures for GHDL..."

test "$DEBUG" -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure SetupDirectories( <Index> <Name> )${ANSI_NOCOLOR}"
# SetupDirectories
# -> $Index
# -> $Name
# <= $SourceDirectory
# <= $DestinationDirectory
SetupDirectories() {
	local Index=$1
	local Name=$2

	declare -n Settings="${Index}_Settings"

	# source directory
	# ----------------------
	# If a command line argument ('--source') was passed in, use it, else use the default value
	# from config.sh
	if [[ ! -z "$SrcDir" ]]; then
		SourceDirectory=${SrcDir%/}										# remove trailing slashes
	elif [[ ! -z "$EnvSourceDir" ]]; then
		SourceDirectory=$EnvSourceDir									# fall back to environment variable
	elif [[ ! -z "${Settings[InstallationDirectory]}" ]]; then
		SourceDirectory=${Settings[InstallationDirectory]}/${Settings[SourceDirectory]}	# fall back to value from config.sh
	fi
	# output directory
	# ----------------------
	# If a command line argument ('--output') was passed in, use it, else use the default value
	# from config.sh
	if [[ ! -z "$DestDir" ]]; then
		DestinationDirectory=${DestDir%/}												# remove trailing slashes
	else
		DestinationDirectory=${Settings[DestinationDirectory]}	# fall back to value from config.sh
	fi

	if [[ -z $SourceDirectory || -z $DestinationDirectory ]]; then
		echo 1>&2 -e "${COLORED_ERROR} $Name is not configured in '$ScriptDir/config.sh'.${ANSI_NOCOLOR}"
		echo 1>&2 -e "  Use adv. options '--source' and '--output' or configure 'config.sh'."
		exit 1
	elif [[ ! -d $SourceDirectory ]]; then
		echo 1>&2 -e "${COLORED_ERROR} Path '$SourceDirectory' does not exist.${ANSI_NOCOLOR}"
		exit 1
	fi

	# Resolve paths to an absolute paths
	test greadlink --version > /dev/null 2>&1 && READLINK=greadlink || READLINK=readlink
	SourceDirectory=$($READLINK -f $SourceDirectory)
	if [[ ! "$DestinationDirectory" = /* ]]; then
		DestinationDirectory="$WorkingDir/$DestinationDirectory"
	fi
}

test "$DEBUG" -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure CreateDestinationDirectory( undocumented )${ANSI_NOCOLOR}"
# CreateDestinationDirectory
# -> undocumented
CreateDestinationDirectory() {
	if [ -d "$DestinationDirectory" ]; then
		echo -e "${COLORED_WARNING} Vendor directory '$DestinationDirectory' already exists.${ANSI_NOCOLOR}"
	elif [ -f "$DestinationDirectory" ]; then
		echo 1>&2 -e "${COLORED_ERROR} Vendor directory '$DestinationDirectory' already exists as a file.${ANSI_NOCOLOR}"
		exit 1
	else
		echo -e "Creating vendor directory: '$DestinationDirectory'.${ANSI_NOCOLOR}"
		mkdir -p "$DestinationDirectory"
	fi
}

test "$DEBUG" -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure GHDLSetup( <VHDLStandard> )${ANSI_NOCOLOR}"
# GHDLSetup
# -> $VHDLStandard
# <= $VHDLVersion
# <= $VHDLStandard
# <= $VHDLFlavor
GHDLSetup() {
	if [ "$1" -eq 93 ]; then
		VHDLVersion="v93"
		VHDLStandard="93c"
		VHDLFlavor="synopsys"
	elif [ "$1" -eq 2008 ]; then
		VHDLVersion="v08"
		VHDLStandard="08"
		VHDLFlavor="synopsys"
	fi
}

test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure CreateVHDLLibrary( <StructName> <LibraryName> <LibraryPath> <VHDLVersion> <Files[*]> )${ANSI_NOCOLOR}"
# CreateLibraryStruct
# -> $StructName
# -> $LibraryName
# -> $LibraryPath
# -> $VHDLVersion
# -> $Files[*]
CreateLibraryStruct() {
	local StructName=$1; shift

	declare -g "${StructName}_LibraryName"=$1; shift
	declare -g "${StructName}_LibraryPath"=$1; shift
	declare -g "${StructName}_VHDLVersion"=$1; shift

	declare -n FilesRef="${StructName}_Files"
	FilesRef=( "$*" )
}

test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure DeleteLibraryStruct( <StructName> )${ANSI_NOCOLOR}"
# DeleteLibraryStruct
# -> $StructName
DeleteLibraryStruct() {
	local StructName=$1

	unset "${StructName}_VHDLVersion"
	unset "${StructName}_LibraryName"
	unset "${StructName}_LibraryPath"
	unset "${StructName}_Files"
}

test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure PrintLibraryStruct( <StructName> )${ANSI_NOCOLOR}"
# PrintLibraryStruct
# -> $StructName
PrintLibraryStruct() {
	local StructName=$1
	local Indentation=${2:-"    "}

	local LibraryName="${StructName}_LibraryName"; local LibraryName=${!LibraryName}
	local Files="${StructName}_Files[*]";          local Files=${!Files}

	echo -e "$Indentation${ANSI_DARK_GRAY}VHDL Library name: $LibraryName${ANSI_NOCOLOR}"
	for File in ${Files[*]}; do
		echo -e "$Indentation  ${ANSI_DARK_GRAY}$File${ANSI_NOCOLOR}"
	done
}


declare -A GHDLLibraryMapping

test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure CreateVHDLLibrary( <LibraryName> <DirectoryName> <VHDLVersion> )${ANSI_NOCOLOR}"
# CreateVHDLLibrary
# -> $LibraryName
# -> $DirectoryName
# -> $VHDLVersion
CreateVHDLLibrary() {
	local LibraryName=$1
	local DirectoryName=$2
	local VHDLVersion=${3:-"v08"}

	echo -e "${ANSI_YELLOW}Creating VHDL Library '$LibraryName'...${ANSI_NOCOLOR}"

	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}mkdir -p \"$DirectoryName/$VHDLVersion\"${ANSI_NOCOLOR}"
	mkdir -p "$DirectoryName/$VHDLVersion"

	LibraryDir="$(pwd)/$DirectoryName/$VHDLVersion"
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Mapping library $LibraryName to '$LibraryDir'.${ANSI_NOCOLOR}"
	GHDLLibraryMapping[$LibraryName]=$LibraryDir
}

test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure AnalyzeVHDL( <LibraryName> <SourceDirectory> <LibraryPath> <File> )${ANSI_NOCOLOR}"
# AnalyzeVHDL
# -> $LibraryName
# -> $SourceDirectory
# -> $LibraryPath
# -> $File
AnalyzeVHDL() {
	local LibraryName=$1
	local SourceDirectory=$2
	local LibraryPath=$3
	local File=$4

	local DestinationDirectory=${GHDLLibraryMapping[$LibraryName]}

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

	local SourceFile="$SourceDirectory/$LibraryPath/$File"

	if [[ ! -f "$SourceFile" ]]; then
		echo 1>&2 -e "${COLORED_ERROR} Source file '$SourceFile' not found.${ANSI_NOCOLOR}"
		test $CONTINUE_ON_ERROR -eq 0 && exit 1
	fi

	if [[ $FILTERING -eq 0 ]]; then
		test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}$GHDL -a ${Analyze_Parameters[*]} ${Parameters[*]} --work=$LibraryName --workdir=$DestinationDirectory \"$SourceFile\"${ANSI_NOCOLOR}"
		$GHDL -a "${Analyze_Parameters[@]}" "${Parameters[@]}" --work=$LibraryName --workdir=$DestinationDirectory "$SourceFile"
		ExitCode=$?
		if [[ $ExitCode -ne 0 ]]; then
			echo 1>&2 -e "$Filter_Indent${COLORED_ERROR} While analyzing '$File'. ExitCode: $ExitCode${ANSI_NOCOLOR}"
			test $CONTINUE_ON_ERROR -eq 0 && exit 1
		fi
	 else
		 test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}$GHDL -a ${Analyze_Parameters[*]} ${Parameters[*]} --work=$LibraryName --workdir=$DestinationDirectory \"$SourceFile\" 2>&1 | \\\\${ANSI_NOCOLOR}"
		 test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}$ScriptDir/$Analyze_Filter ${Filter_Parameters[*]} -i \"$Filter_Indent\"${ANSI_NOCOLOR}"
		 $GHDL -a "${Analyze_Parameters[@]}" "${Parameters[@]}" --work=$LibraryName --workdir=$DestinationDirectory "$SourceFile" 2>&1 | $ScriptDir/$Analyze_Filter "${Filter_Parameters[@]}" -i "$Filter_Indent"
		 local PiplineStatus=("${PIPESTATUS[@]}")
		 if [[ ${PiplineStatus[0]}  -ne 0 ]]; then
			 echo 1>&2 -e "$Filter_Indent${COLORED_ERROR} While analyzing '$File'. ExitCode: ${PiplineStatus[0]}${ANSI_NOCOLOR}"
			 test $CONTINUE_ON_ERROR -eq 0 && exit 1
		 elif [[ ${PiplineStatus[1]}  -ne 0 ]]; then
			 case $(( ${PiplineStatus[1]} % 4 )) in
				 # TODO: implement CONTINUE_ON_ERROR in cases ...
				 3) echo 1>&2 -e "$Filter_Indent${ANSI_RED}Fatal errors detected by filtering script. ExitCode: ${PiplineStatus[1]}${ANSI_NOCOLOR}"; exit 1 ;;
				 2) echo 1>&2 -e "$Filter_Indent${ANSI_RED}Errors detected by filtering script. ExitCode: ${PiplineStatus[1]}${ANSI_NOCOLOR}"; exit 1 ;;
				 1) echo 1>&2 -e "$Filter_Indent${ANSI_YELLOW}Warnings detected by filtering script.${ANSI_NOCOLOR}" ;;
				 0) test $DEBUG -eq 1 && echo 1>&2 -e "$Filter_Indent${ANSI_YELLOW}Warnings detected by filtering script.${ANSI_NOCOLOR}" ;;
			 esac
		 fi
	fi
}


test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure AnalyzeLibrary( <LibraryName> <SourceDirectory> <LibraryPath> <Files[*]> )${ANSI_NOCOLOR}"
# AnalyzeLibrary
# -> LibraryName
# -> SourceDirectory
# -> LibraryPath
# -> Files[*]
AnalyzeLibrary() {
	local LibraryName=$1;     shift
	local SourceDirectory=$1; shift
	local LibraryPath=$1;     shift
	local Files=$@

	echo -e "${ANSI_YELLOW}Analyzing files into library '$LibraryName'...${ANSI_NOCOLOR}"

	for File in $Files; do
		test "$VERBOSE" -eq 1 && echo -e "${ANSI_CYAN}  Analyzing '$File'${ANSI_NOCOLOR}"

		AnalyzeVHDL "$LibraryName" "$SourceDirectory" "$LibraryPath" "$File"
	done
}

test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}procedure Compile( <SourceDirectory> <Libraries> )${ANSI_NOCOLOR}"
# Compile
# -> SourceDirectory
# -> VHDLLibraries
Compile() {
	local SourceDirectory=$1
	local VHDLLibraries=$2

	for VHDLLibrary in $VHDLLibraries; do
		local LibraryName="${VHDLLibrary}_LibraryName"; local LibraryName=${!LibraryName}
		local LibraryPath="${VHDLLibrary}_LibraryPath"; local LibraryPath=${!LibraryPath}
		local VHDLVersion="${VHDLLibrary}_VHDLVersion"; local VHDLVersion=${!VHDLVersion}
		local Files="${VHDLLibrary}_Files[*]";          local Files=${!Files}

		echo -e "${ANSI_LIGHT_CYAN}Analyzing library '$LibraryName'...${ANSI_NOCOLOR}"

		CreateVHDLLibrary $LibraryName $LibraryName $VHDLVersion
		AnalyzeLibrary $LibraryName "$SourceDirectory" "$LibraryPath" "$Files"
	done
}
