#!/bin/bash

set -e

if [ -e gnat/etc/install_ok ] && [ "x$(cat gnat/etc/install_ok)" = "x2019" ]; then
    echo "gnatgpl already installed"
    exit 0
fi

set -x

# Remove old gnat directory
if [ -d gnat ]; then
    rm -rf gnat
fi

# Download from community.adacore.com and extract
wget -q --show-progress --progress=bar:force:noscroll -O dmgfile https://community.download.adacore.com/v1/5a7801fc686e86de838cfaf7071170152d81254d?filename=gnat-community-2019-20190517-x86_64-darwin-bin.dmg
7z x dmgfile
installer="gnat-community-2019-20190517-x86_64-darwin-bin/gnat-community-2019-20190517-x86_64-darwin-bin.app/Contents/MacOS/gnat-community-2019-20190517-x86_64-darwin-bin"

# Install
mkdir -p gnat
chmod +x $installer
./$installer PREFIX=gnat

echo "2019" > gnat/etc/install_ok
