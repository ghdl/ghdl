#!/bin/bash

set -e

if [ -e gnat/etc/install_ok ]; then
    echo "gnatgpl already installed"
    exit 0
fi

echo "Download and install gnat-gpl"
set -x

# Download from libre.adacore.com
tarfile=gnat-gpl-2017-x86_64-darwin-bin.tar.gz
curl -o $tarfile http://mirrors.cdn.adacore.com/art/591c9045c7a447af2deed24e

# un tar
tar xf $tarfile

# Remove old gnat directory and install manually
rm -rf gnat
mv gnat-gpl-2017-x86_64-darwin-bin gnat

# Cleanup: remove components not needed
rm -rf gnat/share/{themes,icons}
rm -rf gnat/share/{man,info,doc,examples,gpr,gprconfig,gnatcoll}
rm -rf gnat/share/gps
rm -rf gnat/share/gdb* gnat/share/glib* gnat/share/gcc-*/python
rm -rf gnat/etc/fonts gnat/etc/gtk*

rm -f gnat/bin/aws* gnat/bin/gps* gnat/bin/gcov*
rm -f gnat/bin/gnat2* gnat/bin/xml2* gnat/bin/gnatcoll*
rm -f gnat/bin/gnat{doc,metric,pp,stub,prep,test,check,elim,inspect,find,kr}
rm -f gnat/bin/gnat{xref,name}
rm -f gnat/bin/gpr* gnat/bin/templates* gnat/bin/web* gnat/bin/wsdl*
rm -f gnat/bin/{gdb,cpp,c++,g++} gnat/bin/x86_64-*

rm -rf gnat/include/{asis,aunit,gnat_util,gnatcoll,gpr,xmlada}
rm -rf gnat/include/aws* gnat/include/pycairo gnat/include/python*
rm -rf gnat/include/pygobject* gnat/include/gdb
rm -rf gnat/include/c++

rm -f gnat/lib/libcc1* gnat/lib/libgomp* gnat/lib/libitm* gnat/lib/libasan*
rm -f gnat/lib/libatomic* gnat/lib/libobjc* gnat/lib/libssp*
rm -f gnat/lib/libstdc++* gnat/lib/libubsan* gnat/lib/libsupc++*
rm -f gnat/lib/libxmlada*
rm -rf gnat/lib/{aunit,gnat,gnat_util,gnatcoll,gpr,gps,xmlada}
rm -rf gnat/lib/aws* gnat/lib/girepository* gnat/lib/gtk*
rm -rf gnat/lib/python*
rm -rf gnat/lib/gcc/x86*/*/{gcc-include,plugin,install-tools}
rm -rf gnat/lib/gcc/x86*/*/rts-ios-simulator
rm -rf gnat/lib/gcc/x86*/*/rts-native/adalib/lib*.dSYM
rm -rf gnat/lib/gcc/x86*/*/rts-native/adalib/*.dylib
rm -rf gnat/lib/gcc/x86*/*/rts-native/adalib/lib*_pic.a

rm -rf gnat/libexec/gprbuild
rm -rf gnat/libexec/gcc/x86*/*/{plugin,install-tools}
rm -f gnat/libexec/gcc/x86*/*/{cc1obj,cc1plus,lto1}

touch gnat/etc/install_ok
