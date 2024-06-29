#!/bin/bash

set -e

echo "ARCH: $RUNNER_ARCH"

case "$RUNNER_ARCH" in
    X64)
	gnat_ver=12.1.0-2
	gnat_arch=x86_64
	;;
    ARM64)
	gnat_ver=14.1.0-3
	gnat_arch=aarch64
	;;
    *)
	echo "Unknown arch ($RUNNER_ARCH)"
	exit 1
	;;
esac

gnat_name="gnat-${gnat_arch}-darwin-${gnat_ver}"
gnat_url="https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnat-${gnat_ver}/${gnat_name}.tar.gz"

if [ -e gnat/install_ok ] && [ "$(cat gnat/install_ok)" = "$gnat_name" ]; then
    echo "gnat already installed"
    exit 0
fi

set -x

# Remove old gnat directory
if [ -d gnat ]; then
    rm -rf gnat
fi

# Download from community.adacore.com and extract
wget -q --show-progress --progress=bar:force:noscroll "$gnat_url"
tar zxf ${gnat_name}.tar.gz
mv ${gnat_name} gnat

./gnat/bin/gnatls -v

rm ${gnat_name}.tar.gz

echo "$gnat_name" > gnat/install_ok
