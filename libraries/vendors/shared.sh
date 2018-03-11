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

# red texts
COLORED_ERROR="$ANSI_RED[ERROR]$ANSI_NOCOLOR"
COLORED_FAILED="$ANSI_RED[FAILED]$ANSI_NOCOLOR"

# green texts
COLORED_DONE="$ANSI_GREEN[DONE]$ANSI_NOCOLOR"
COLORED_SUCCESSFUL="$ANSI_GREEN[SUCCESSFUL]$ANSI_NOCOLOR"

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
		exit -1
	elif [ ! -d $SourceDirectory ]; then
		echo 1>&2 -e "${COLORED_ERROR} Path '$SourceDirectory' does not exist.${ANSI_NOCOLOR}"
		exit -1
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
			exit -1
		fi
	elif [ ! -z "$GHDL" ]; then
		if [ ! \( -f "$GHDL" -a -x "$GHDL" \) ]; then
			echo 1>&2 -e "${COLORED_ERROR} Found GHDL environment variable, but '$GHDL' is not executable.${ANSI_NOCOLOR}"
			exit -1
		fi
		GHDLBinary=$GHDL
	else	# fall back to GHDL found via PATH
		GHDLBinary=$(which ghdl 2>/dev/null)
		if [ $? -ne 0 ]; then
			echo 1>&2 -e "${COLORED_ERROR} GHDL not found in PATH.${ANSI_NOCOLOR}"
			echo 1>&2 -e "  Use adv. options '--ghdl' to set the GHDL binary directory."
			exit -1
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
		exit -1
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

