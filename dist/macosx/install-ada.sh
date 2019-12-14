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
rm -rf gnat/share/{themes,icons} \
       gnat/share/{man,info,doc,examples,gpr,gprconfig,gnatcoll} \
       gnat/share/gps \
       gnat/share/gdb* gnat/share/glib* gnat/share/gcc-*/python \
       gnat/etc/fonts gnat/etc/gtk* \
       gnat/include/{asis,aunit,gnat_util,gnatcoll,gpr,xmlada} \
       gnat/include/aws* gnat/include/pycairo gnat/include/python* \
       gnat/include/pygobject* gnat/include/gdb \
       gnat/include/c++ \
       gnat/lib/{aunit,gnat,gnat_util,gnatcoll,gpr,gps,xmlada} \
       gnat/lib/aws* gnat/lib/girepository* gnat/lib/gtk* \
       gnat/lib/python* \
       gnat/lib/gcc/x86*/*/{gcc-include,plugin,install-tools} \
       gnat/lib/gcc/x86*/*/rts-ios-simulator \
       gnat/lib/gcc/x86*/*/rts-native/adalib/lib*.dSYM \
       gnat/lib/gcc/x86*/*/rts-native/adalib/*.dylib \
       gnat/lib/gcc/x86*/*/rts-native/adalib/lib*_pic.a \
       gnat/libexec/gprbuild \
       gnat/libexec/gcc/x86*/*/{plugin,install-tools}

rm -f  gnat/bin/aws* gnat/bin/gps* gnat/bin/gcov* \
       gnat/bin/gnat2* gnat/bin/xml2* gnat/bin/gnatcoll* \
       gnat/bin/gnat{doc,metric,pp,stub,prep,test,check,elim,inspect,find,kr} \
       gnat/bin/gnat{xref,name} \
       gnat/bin/gpr* gnat/bin/templates* gnat/bin/web* gnat/bin/wsdl* \
       gnat/bin/{gdb,cpp,c++,g++} gnat/bin/x86_64-* \
       gnat/lib/libcc1* gnat/lib/libgomp* gnat/lib/libitm* gnat/lib/libasan* \
       gnat/lib/libatomic* gnat/lib/libobjc* gnat/lib/libssp* \
       gnat/lib/libstdc++* gnat/lib/libubsan* gnat/lib/libsupc++* \
       gnat/lib/libxmlada* \
       gnat/libexec/gcc/x86*/*/{cc1obj,cc1plus,lto1}

touch gnat/etc/install_ok
