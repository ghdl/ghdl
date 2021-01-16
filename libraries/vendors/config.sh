# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
#  Bash Script:  Configurable directories to local installed tools
#
# Description:
# ------------------------------------
#  This Bash file exports variables containing the users local tool environment.
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

# Configure
# - Vendor tool chain installation paths or
# - Library root directories
# in the following dictionaries.
#
# These values are used if no command line argument (--source, --output) is
# passed to a compile script. An empty 'InstallationDirectory' string means not
# configured. Declare source directories depending on the installation paths of
# the 3rd party tools. Configure preferred output directories for each library
# set.
#
declare -A Altera_Quartus_Settings=(
	["InstallationDirectory"]=""              # "/opt/altera/16.0/quartus"
	["SourceDirectory"]="eda/sim_lib"
	["DestinationDirectory"]="altera"
)

declare -A Intel_Quartus_Settings=(
	["InstallationDirectory"]=""              # "/opt/intelFPGA/20.1/quartus"
	["SourceDirectory"]="eda/sim_lib"
	["DestinationDirectory"]="intel"
)

declare -A Lattice_Diamond_Settings=(
	["InstallationDirectory"]=""              # "/usr/local/diamond/3.10_x64"
	["SourceDirectory"]="cae_library/simulation/vhdl"
	["DestinationDirectory"]="lattice"
)

declare -A OSVVM_Settings=(
	["InstallationDirectory"]=""              # "~/git/github/OSVVM"
	["SourceDirectory"]="."
	["DestinationDirectory"]="."
)

declare -A UVVM_Settings=(
	["InstallationDirectory"]=""              # "~/git/github/UVVM"
	["SourceDirectory"]="."
	["DestinationDirectory"]="."
)

declare -A Xilinx_ISE_Settings=(
	["InstallationDirectory"]=""              # "/opt/Xilinx/14.7/ISE_DS"
	["SourceDirectory"]="ISE/vhdl/src"
	["DestinationDirectory"]="xilinx-ise"
)

declare -A Xilinx_Vivado_Settings=(
	["InstallationDirectory"]=""              # "/opt/Xilinx/Vivado/2020.2"
	["SourceDirectory"]="data/vhdl/src"
	["DestinationDirectory"]="xilinx-vivado"
)


# input files greater than $LARGE_FILESIZE are skipped if '--skip-largefiles' is set
LARGE_FILESIZE=125000
