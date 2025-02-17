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
#  Copyright (C) 2017-2025 Patrick Lehmann - Boetzingen, Germany
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

VERBOSE=${VERBOSE:-0}
DEBUG=${DEBUG:-0}
CONTINUE_ON_ERROR=${CONTINUE_ON_ERROR:-0}


if [[ -n "$GHDL" ]]; then
	if [[ ! -f "$GHDL" ]]; then
		PrintErrorAndExit "Found GHDL environment variable, but '$GHDL' is not a file."
	elif [[ ! -x "$GHDL" ]]; then
		PrintErrorAndExit "Found GHDL environment variable, but '$GHDL' is not executable."
	fi
else	# fall back to GHDL found via PATH
	GHDL=$(which ghdl 2>/dev/null)
	if [[ $? -ne 0 ]]; then
		PrintError           "GHDL not found in PATH."
		ContinueErrorAndExit   "Use adv. options '--ghdl' to set the GHDL binary directory."
	fi
fi

Analyze_Filter=filter.analyze.sh
Analyze_Parameters=(
	--mb-comments
)

PrintDebug "  " "Declaring Bash procedures for GHDL..."
DeclareProcedure "SetupDirectories" "<Index> <Name>"
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
		PrintError           "$Name is not configured in '$ScriptDir/config.sh'."
		ContinueErrorAndExit   "Use adv. options '--source' and '--output' or configure 'config.sh'."
	elif [[ ! -d $SourceDirectory ]]; then
		PrintErrorAndExit "Path '$SourceDirectory' does not exist."
	fi

	# Resolve paths to an absolute paths
	test greadlink --version > /dev/null 2>&1 && READLINK=greadlink || READLINK=readlink
	SourceDirectory=$($READLINK -f $SourceDirectory)
	if [[ ! "$DestinationDirectory" = /* ]]; then
		DestinationDirectory="$WorkingDir/$DestinationDirectory"
	fi
}

DeclareProcedure "CreateDestinationDirectory" "undocumented"
# CreateDestinationDirectory
# -> undocumented
CreateDestinationDirectory() {
	if [ -d "$DestinationDirectory" ]; then
		PrintWarning "Vendor directory '$DestinationDirectory' already exists."
	elif [ -f "$DestinationDirectory" ]; then
		PrintErrorAndExit "Vendor directory '$DestinationDirectory' already exists as a file."
	else
		printf "%s\n" "Creating vendor directory: '$DestinationDirectory'.${ANSI_NOCOLOR}"
		mkdir -p "$DestinationDirectory"
	fi
}

DeclareProcedure "GHDLSetup" "<VHDLStandard>"
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

DeclareProcedure "CreateVHDLLibrary" "<StructName> <LibraryName> <LibraryPath> <VHDLVersion> <Files[*]>"
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

DeclareProcedure "DeleteLibraryStruct" "<StructName>"
# DeleteLibraryStruct
# -> $StructName
DeleteLibraryStruct() {
	local StructName=$1

	unset "${StructName}_VHDLVersion"
	unset "${StructName}_LibraryName"
	unset "${StructName}_LibraryPath"
	unset "${StructName}_Files"
}

DeclareProcedure "PrintLibraryStruct" "<StructName>"
# PrintLibraryStruct
# -> $StructName
PrintLibraryStruct() {
	local StructName=$1
	local Indentation=${2:-"    "}

	local LibraryName="${StructName}_LibraryName"; local LibraryName=${!LibraryName}
	local Files="${StructName}_Files[*]";          local Files=${!Files}

	PrintDebug "$Indentation" "VHDL Library name: ${LibraryName}"
	for File in ${Files[*]}; do
		PrintDebug "$Indentation  " "${File}"
	done
}


declare -A GHDLLibraryMapping

DeclareProcedure "CreateVHDLLibrary" "<LibraryName> <DirectoryName> <VHDLVersion>"
# CreateVHDLLibrary
# -> $LibraryName
# -> $DirectoryName
# -> $VHDLVersion
CreateVHDLLibrary() {
	local LibraryName=$1
	local DirectoryName=$2
	local VHDLVersion=${3:-"v08"}

	SubSection "Creating VHDL Library '${LibraryName}'..."
	PrintVerbose "Creating library directory '$DirectoryName/$VHDLVersion'"
	PrintDebug "mkdir -p \"$DirectoryName/$VHDLVersion\""
	mkdir -p "$DirectoryName/$VHDLVersion"

	LibraryDir="$(pwd)/$DirectoryName/$VHDLVersion"
	PrintDebug "Mapping library $LibraryName to '$LibraryDir'."
	GHDLLibraryMapping[$LibraryName]=$LibraryDir
}

DeclareProcedure "AnalyzeVHDL" "<LibraryName> <SourceDirectory> <LibraryPath> <File>"
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

	if [[ ! -f "${SourceFile}" ]]; then
		CheckErrorOrContinue 0 $CONTINUE_ON_ERROR "Source file '${SourceFile}' not found."
	fi

	if [[ $FILTERING -eq 0 ]]; then
		PrintDebug "$GHDL -a ${Analyze_Parameters[*]} ${Parameters[*]} --work=$LibraryName --workdir=$DestinationDirectory \"$SourceFile\""
		$GHDL -a "${Analyze_Parameters[@]}" "${Parameters[@]}" --work=$LibraryName --workdir=$DestinationDirectory "$SourceFile"
		CheckErrorOrContinue $? $CONTINUE_ON_ERROR "${Filter_Indent}" "While analyzing '$File'."
	 else
		 PrintDebug "$GHDL -a ${Analyze_Parameters[*]} ${Parameters[*]} --work=$LibraryName --workdir=$DestinationDirectory \"$SourceFile\" 2>&1 | \\\\"
		 PrintDebug "$ScriptDir/$Analyze_Filter ${Filter_Parameters[*]} -i \"$Filter_Indent\""
		 $GHDL -a "${Analyze_Parameters[@]}" "${Parameters[@]}" --work=$LibraryName --workdir=$DestinationDirectory "$SourceFile" 2>&1 | $ScriptDir/$Analyze_Filter "${Filter_Parameters[@]}" -i "$Filter_Indent"
		 local PiplineStatus=("${PIPESTATUS[@]}")
		 if [[ ${PiplineStatus[0]}  -ne 0 ]]; then
		 	 CheckErrorOrContinue ${PiplineStatus[0]} $CONTINUE_ON_ERROR "${Filter_Indent}" "While analyzing '$File'."
		 elif [[ ${PiplineStatus[1]}  -ne 0 ]]; then
			 case $(( ${PiplineStatus[1]} % 4 )) in
				 # TODO: implement CONTINUE_ON_ERROR in cases ...
				 3) printf "$Filter_Indent${ANSI_RED}Fatal errors detected by filtering script. ExitCode: %s${ANSI_NOCOLOR}\n" "${PiplineStatus[1]}"; exit 1 ;;
				 2) printf "$Filter_Indent${ANSI_RED}Errors detected by filtering script. ExitCode: %s${ANSI_NOCOLOR}\n"       "${PiplineStatus[1]}"; exit 1 ;;
				 1) printf "$Filter_Indent${ANSI_YELLOW}Warnings detected by filtering script.${ANSI_NOCOLOR}\n"                                             ;;
				 0) test $DEBUG -eq 1 && printf "$Filter_Indent${ANSI_YELLOW}Warnings detected by filtering script.${ANSI_NOCOLOR}" 1>&2                     ;;
			 esac
		 fi
	fi
}


DeclareProcedure "AnalyzeLibrary" "<LibraryName> <SourceDirectory> <LibraryPath> <Files[*]>"
# AnalyzeLibrary
# -> LibraryName
# -> SourceDirectory
# -> LibraryPath
# -> Files[*]
AnalyzeLibrary() {
	local LibraryName=$1;     shift
	local SourceDirectory=$1; shift
	local LibraryPath=$1;     shift
	local Files="$@"

	SubSection "Analyzing files into library '${LibraryName}'..."
	for File in $Files; do
		PrintVerbose "Analyzing '$File'"
		AnalyzeVHDL "$LibraryName" "$SourceDirectory" "$LibraryPath" "$File"
	done
}

DeclareProcedure "Compile" "<SourceDirectory> <Libraries>"
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

		Section "Analyzing library '$LibraryName'..."

		CreateVHDLLibrary $LibraryName $LibraryName $VHDLVersion
		AnalyzeLibrary $LibraryName "$SourceDirectory" "$LibraryPath" "$Files"
	done
}
