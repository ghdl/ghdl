#!/bin/sh

#  Script used to create tar balls.
#  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
#
#  GHDL is free software; you can redistribute it and/or modify it under
#  the terms of the GNU General Public License as published by the Free
#  Software Foundation; either version 2, or (at your option) any later
#  version.
#
#  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#  for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with GCC; see the file COPYING.  If not, write to the Free
#  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#  02111-1307, USA.

# Building a distribution:
# * update the 'version' variable in ../../Makefile
# * Regenerate version.ads: make -f ../../Makefile version.ads
# * Check NEWS, README and INSTALL files.
# * Check version and copyright years in doc/ghdl.texi, ghdlmain.adb
# * Check GCCVERSION below.
# * Check lists of exported files in this file.
# * Create source tar and build binaries: ./dist.sh dist_phase1
# * su root
# * Build binary tar: ./dist.sh dist_phase2
# * Run the testsuites: GHDL=ghdl ./testsuite.sh
# * Update website/index.html (./dist.sh website helps, rename .new)
# * upload (./dist upload)
# * CVS commit, tag + cd image.
# * remove previous version in /usr/local

## DO NOT MODIFY this file while it is running...

set -e

# GTKWave version.
GTKWAVE_VERSION=1.3.72

# GHDL version (extracted from version.ads)
VERSION=`sed -n -e 's/.*GHDL \([0-9.a-z]*\) (.*/\1/p' ../../version.ads`

CWD=`pwd`

distdir=ghdl-$VERSION
tarfile=$distdir.tar
zipfile=$distdir.zip

PREFIX=/usr/local
bindirname=ghdl-$VERSION-i686-pc-linux
TARINSTALL=$DISTDIR/$bindirname.tar.bz2
VHDLDIR=$distdir/vhdl
DOWNLOAD_HTML=../../website/download.html
DESTDIR=$CWD/
UNSTRIPDIR=${distdir}-unstripped

PATH=/usr/gnat/bin:$PATH

do_clean ()
{
  rm -rf $distdir
  mkdir $distdir
  mkdir $distdir/ghdl
  mkdir $distdir/ghdldrv
  mkdir $distdir/libraries
  mkdir $distdir/libraries/std $distdir/libraries/ieee
  mkdir $distdir/libraries/vital95 $distdir/libraries/vital2000
  mkdir $distdir/libraries/synopsys $distdir/libraries/mentor
  mkdir $distdir/grt
  mkdir $distdir/grt/config
  mkdir $distdir/ortho
  mkdir $distdir/windows
}

# Build Makefile
do_Makefile ()
{
  sed -e "/^####libraries Makefile.inc/r ../../libraries/Makefile.inc" \
      -e "/^####grt Makefile.inc/r ../grt/Makefile.inc" \
     < Makefile.in > $distdir/Makefile
}

# Copy (or link) sources files into $distdir
do_files ()
{
. ../gcc/dist-common.sh

ortho_mcode_files="
binary_file-elf.adb
binary_file-elf.ads
binary_file-memory.adb
binary_file-memory.ads
binary_file.adb
binary_file.ads
disa_x86.adb
disa_x86.ads
disassemble.ads
dwarf.ads
elf32.adb
elf32.ads
elf64.ads
elf_common.adb
elf_common.ads
elf_arch32.ads
elf_arch.ads
hex_images.adb
hex_images.ads
memsegs.ads
memsegs_mmap.ads
memsegs_mmap.adb
memsegs_c.c
ortho_code-abi.ads
ortho_code-binary.adb
ortho_code-binary.ads
ortho_code-consts.adb
ortho_code-consts.ads
ortho_code-debug.adb
ortho_code-debug.ads
ortho_code-decls.adb
ortho_code-decls.ads
ortho_code-disps.adb
ortho_code-disps.ads
ortho_code-dwarf.adb
ortho_code-dwarf.ads
ortho_code-exprs.adb
ortho_code-exprs.ads
ortho_code-flags.ads
ortho_code-opts.adb
ortho_code-opts.ads
ortho_code-types.adb
ortho_code-types.ads
ortho_code-x86-emits.adb
ortho_code-x86-emits.ads
ortho_code-x86-insns.adb
ortho_code-x86-insns.ads
ortho_code-x86-abi.adb
ortho_code-x86-abi.ads
ortho_code-x86.adb
ortho_code-x86.ads
ortho_code.ads
ortho_code_main.adb
ortho_ident.adb
ortho_ident.ads
ortho_mcode.adb
ortho_mcode.ads
ortho_nodes.ads
"

windows_files="
compile.bat
complib.bat
default_pathes.ads
ghdl.nsi
windows_default_path.adb
windows_default_path.ads
ghdlfilter.adb
grt-modules.adb
"

for i in $cfiles; do ln -sf $CWD/../../$i $distdir/ghdl/$i; done
for i in $tfiles; do ln -sf $CWD/../$i $distdir/ghdl/$i; done

ln -sf $CWD/../../doc/ghdl.texi $distdir/ghdl.texi

for i in $ortho_files; do ln -sf $CWD/../../ortho/$i $distdir/ortho/$i; done

for i in $ortho_mcode_files; do
  ln -sf $CWD/../../ortho/mcode/$i $distdir/ortho/$i
done

for i in $ghdl_files ghdlcomp.ads ghdlcomp.adb \
  ghdlrun.adb ghdlrun.ads ghdl_mcode.adb; do
  ln -sf $CWD/../ghdldrv/$i $distdir/ghdldrv/$i
done

for i in $libraries_files; do
    ln -sf $CWD/../../libraries/$i $distdir/libraries/$i
done

for i in $grt_files; do
    ln -sf $CWD/../grt/$i $distdir/grt/$i
done

for i in $grt_config_files; do
    ln -sf $CWD/../grt/config/$i $distdir/grt/config/$i
done

for i in $windows_files; do
    ln -sf $CWD/windows/$i $distdir/windows/$i
done

    ln -sf $CWD/winbuild.bat $distdir/winbuild.bat

makeinfo --no-split -o $distdir/windows/ghdl.htm $CWD/../../doc/ghdl.texi 
}

do_sources_dir ()
{
    \rm -rf $distdir
    mkdir $distdir
    do_clean
    do_Makefile
    do_files
    ln -sf ../../../COPYING $distdir
}

# Create the tar of sources.
do_tar ()
{
    do_sources_dir
    tar cvhf $tarfile $distdir
    bzip2 -f $tarfile
    rm -rf $distdir
}

# Create the zip of sources.
do_zip ()
{
    do_sources_dir
    zip -r $zipfile $distdir
    rm -rf $distdir
}

# Extract the source, configure and make.
do_compile ()
{
  set -x

  do_update_gcc_sources;

  rm -rf $GCCDISTOBJ
  mkdir $GCCDISTOBJ
  cd $GCCDISTOBJ
  ../gcc-$GCCVERSION/configure --enable-languages=vhdl --prefix=$PREFIX
  make CFLAGS="-O -g"
  make -C gcc vhdl.info
  cd $CWD
}

check_root ()
{
  if [ $UID -ne 0 ]; then
    echo "$0: you must be root";
    exit 1;
  fi
}

#  Do a make install
do_compile2 ()
{
  set -x
  cd $GCCDISTOBJ
  # Check the info file is not empty.
  if [ -s gcc/doc/ghdl.info ]; then
    echo "info file found"
  else
    echo "Error: ghdl.info not found".
    exit 1;
  fi
  mkdir -p $DESTDIR/usr/local || true
  make DESTDIR=$DESTDIR install
  cd $CWD
  if [ -d $UNSTRIPDIR ]; then
     rm -rf $UNSTRIPDIR
  fi
  mkdir $UNSTRIPDIR
  cp ${DESTDIR}${GCCLIBEXECDIR}/ghdl1 ${DESTDIR}${PREFIX}/bin/ghdl $UNSTRIPDIR
  chmod -w $UNSTRIPDIR/*
  strip ${DESTDIR}${GCCLIBEXECDIR}/ghdl1 ${DESTDIR}${PREFIX}/bin/ghdl
}

# Create the tar file from the current installation.
do_tar_install ()
{
  tar -C $DESTDIR -jcvf $TARINSTALL \
    ./$PREFIX/bin/ghdl ./$PREFIX/info/ghdl.info \
    ./$GCCLIBDIR/vhdl \
    ./$GCCLIBEXECDIR/ghdl1
}

do_extract_tar_install ()
{
  check_root;
  cd /
  tar jxvf $TARINSTALL
  cd $CWD
}

# Create the tar file to be distributed.
do_tar_dist ()
{
  rm -rf $bindirname
  mkdir $bindirname
  sed -e "s/@TARFILE@/$dir.tar/" < INSTALL > $bindirname/INSTALL
  ln ../../COPYING $bindirname
  ln $TARINSTALL $bindirname
  tar cvf $bindirname.tar $bindirname
}

# Remove the non-ghdl files of gcc in the current installation.
do_distclean_gcc ()
{
  set -x
  rm -f ${DESTDIR}${PREFIX}/bin/cpp ${DESTDIR}${PREFIX}/bin/gcc
  rm -f ${DESTDIR}${PREFIX}/bin/gccbug ${DESTDIR}${PREFIX}/bin/gcov
  rm -f ${DESTDIR}${PREFIX}/bin/${MACHINE}-gcc*
  rm -f ${DESTDIR}${PREFIX}/info/cpp.info*
  rm -f ${DESTDIR}${PREFIX}/info/cppinternals.info*
  rm -f ${DESTDIR}${PREFIX}/info/gcc.info*
  rm -f ${DESTDIR}${PREFIX}/info/gccinstall.info*
  rm -f ${DESTDIR}${PREFIX}/info/gccint.info*
  rm -f ${DESTDIR}${PREFIX}/lib/*.a ${DESTDIR}${PREFIX}/lib/*.so*
  rm -rf ${DESTDIR}${PREFIX}/share
  rm -rf ${DESTDIR}${PREFIX}/man
  rm -rf ${DESTDIR}${PREFIX}/include
  rm -f ${DESTDIR}${GCCLIBEXECDIR}/cc1 ${DESTDIR}${GCCLIBEXECDIR}/collect2
  rm -f ${DESTDIR}${GCCLIBEXECDIR}/cpp0 ${DESTDIR}${GCCLIBEXECDIR}/tradcpp0
  rm -f ${DESTDIR}${GCCLIBDIR}/*.o ${DESTDIR}$GCCLIBDIR/*.a
  rm -f ${DESTDIR}${GCCLIBDIR}/specs
  rm -rf ${DESTDIR}${GCCLIBDIR}/include
  rm -rf ${DESTDIR}${GCCLIBDIR}/install-tools
  rm -rf ${DESTDIR}${GCCLIBEXECDIR}/install-tools
}

# Remove ghdl files in the current installation.
do_distclean_ghdl ()
{
  check_root;
  set -x
  rm -f $PREFIX/bin/ghdl
  rm -f $PREFIX/info/ghdl.info*
  rm -f $GCCLIBEXECDIR/ghdl1
  rm -rf $GCCLIBDIR/vhdl
}

# Build the source tar, and build the binaries.
do_dist_phase1 ()
{
  do_sources;
  do_compile;
  do_compile2;
  do_distclean_gcc;
  do_tar_install;
  do_tar_dist;
  rm -rf ./$PREFIX
}

# Install the binaries and create the binary tar.
do_dist_phase2 ()
{
  check_root;
  do_distclean_ghdl;
  do_extract_tar_install;
  echo "dist_phase2 success"
}

# Create gtkwave patch
do_gtkwave_patch ()
{
#  rm -rf gtkwave-patch
  mkdir gtkwave-patch
  diff -rc -x Makefile.in $GTKWAVE_BASE.orig $GTKWAVE_BASE | \
    sed -e "/^Only in/d" \
    > gtkwave-patch/gtkwave-$GTKWAVE_VERSION.diffs
  cp ../grt/ghwlib.c ../grt/ghwlib.h $GTKWAVE_BASE/src/ghw.c gtkwave-patch
  sed -e "s/VERSION/$GTKWAVE_VERSION/g" < README.gtkwave > gtkwave-patch/README
  tar zcvf ../../website/gtkwave-patch.tgz gtkwave-patch
  rm -rf gtkwave-patch
}

# Update the index.html
# Update the doc
do_website ()
{
  sed -e "
/SRC-HREF/ s/href=\".*\"/href=\"$tarfile.bz2\"/
/BIN-HREF/ s/href=\".*\"/href=\"$bindirname.tar\"/
/HISTORY/ a \\
      <tr>\\
	<td>$VERSION</td>\\
        <td>`date +'%b %e %Y'`</td>\\
        <td>$GCCVERSION</td>\\
	<td><a href=\"$tarfile.bz2\">$tarfile.bz2</a></td>\\
	<td><a href=\"$bindirname.tar\">\\
	    $bindirname.tar</a></td>\\
      </tr>
" < $DOWNLOAD_HTML > "$DOWNLOAD_HTML".new
  dir=../../website/ghdl
  echo "Updating $dir"
  rm -rf $dir
  makeinfo --html -o $dir ../../doc/ghdl.texi
}

# Do ftp commands to upload
do_upload ()
{
if tty -s; then
  echo -n "Please, enter password: "
  stty -echo
  read pass
  stty echo
  echo
else
  echo "$0: upload must be done from a tty"
  exit 1;
fi
ftp -n <<EOF
open ftpperso.free.fr
user ghdl $pass
prompt
hash
bin
passive
put $tarfile.bz2
put $bindirname.tar
put INSTALL
lcd ../../website
put NEWS
put index.html
put download.html
put features.html
put roadmap.html
put manual.html
put more.html
put links.html
put bug.html
put waveviewer.html
put gtkwave-patch.tgz
put favicon.ico
lcd ghdl
cd ghdl
mput \*
bye
EOF
}

if [ $# -eq 0 ]; then
  do_Makefile;
else
  for i ; do
    case $i in
      clean)
        do_clean ;;
      Makefile|makefile)
	do_Makefile ;;
      files)
        do_files ;;
      tar)
        do_tar ;;
      zip)
        do_zip ;;
      compile)
        do_compile;;
      update_gcc)
        do_update_gcc_sources;;
      compile2)
        do_compile2;;
      tar_install)
        do_tar_install;;
      tar_dist)
        do_tar_dist;;
      -v | --version | version)
        echo $VERSION
        exit 0
        ;;
      website)
        do_website;;
      upload)
        do_upload;;
      distclean_gcc)
        do_distclean_gcc;;
      distclean_ghdl)
        do_distclean_ghdl;;
      dist_phase1)
        do_dist_phase1;;
      dist_phase2)
        do_dist_phase2;;
      gtkwave_patch)
        do_gtkwave_patch;;
      *)
	echo "usage: $0 clean|Makefile|files|all"
	exit 1 ;;
     esac
   done
fi
