#! /bin/bash
# ==============================================================================
# Author: Patrick Lehmann
# ------------------------------------------------------------------------------
# This script is for the Travis-CI deployment process. It generates a gziped
# tarball, which can be uploaded by the travis-deploy agent to for examples
# GitHub releases. The generated file has the format:
#   ghdl-<backend>-<version>.tar.gz
# Example:
#   ghdl-llvm-0.34dev.tar.gz
#
# ==============================================================================
# $1 is the GHDL backend: mcode|gcc|llvm
GHDL_BACKEND="$1"

# $2 is the latest name of the latest git tag
#  examples: v0.33; v0.34dev-2016.04.20
GIT_TAG_NAME="$2"
if [ ${#GIT_TAG_NAME} -eq 5 ]; then
  GHDL_VERSION=${GIT_TAG_NAME:1}
else
  GHDL_VERSION="${GIT_TAG_NAME:1:7}"
fi

CDIR=$PWD

# define color escape codes
RED='\e[0;31m'			# Red
GREEN='\e[1;32m'		# Green
MAGENTA='\e[1;35m'	# Magenta
CYAN='\e[1;36m'			# Cyan
NOCOLOR='\e[0m'			# No Color

echo -e "${MAGENTA}========================================${NOCOLOR}"
echo -e "${MAGENTA}             Deploying GHDL             ${NOCOLOR}"
echo -e "${MAGENTA}========================================${NOCOLOR}"

# Needed variables
PREFIX="$CDIR/install-$GHDL_BACKEND"
GHDL_TARBALL="ghdl-$GHDL_BACKEND-$GHDL_VERSION.tar.gz"

# Prepare the environment
echo -e "${CYAN}cd $PREFIX${NOCOLOR}"
cd $PREFIX

# Creating the package
echo "Creating $GHDL_TARBALL package..."
tar -czf $GHDL_TARBALL *
if [ $? -eq 0 ]; then
  echo -e "${GREEN}Packing [SUCCESSFUL]${NOCOLOR}"
else
  echo 1>&2 -e "${RED}Packing [FAILED]${NOCOLOR}"
  exit 1
fi

# Restore the environment
cd $CDIR
