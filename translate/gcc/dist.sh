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

VERSION=`sed -n -e 's/.*GHDL \([0-9.a-z]*\) (.*/\1/p' ../../version.ads`

CWD=`pwd`

distdir=ghdl-$VERSION
tarfile=$distdir.tar

GCCVERSION=4.0.2
DISTDIR=/home/gingold/dist
GCCDIST=$DISTDIR/gcc-$GCCVERSION
GCCDISTOBJ=$GCCDIST-objs
PREFIX=/usr/local
GCCLIBDIR=$PREFIX/lib/gcc/i686-pc-linux-gnu/$GCCVERSION
GCCLIBEXECDIR=$PREFIX/libexec/gcc/i686-pc-linux-gnu/$GCCVERSION
bindirname=ghdl-$VERSION-i686-pc-linux
TARINSTALL=$DISTDIR/$bindirname.tar.bz2
VHDLDIR=$distdir/vhdl
DOWNLOAD_HTML=../../website/download.html
DESTDIR=$CWD/
UNSTRIPDIR=${distdir}-unstripped

PATH=/usr/gnat/bin:$PATH

do_clean ()
{
  rm -rf $VHDLDIR
  mkdir $VHDLDIR
  mkdir $VHDLDIR/ghdldrv
  mkdir $VHDLDIR/libraries
  mkdir $VHDLDIR/libraries/std $VHDLDIR/libraries/ieee
  mkdir $VHDLDIR/libraries/vital95 $VHDLDIR/libraries/vital2000
  mkdir $VHDLDIR/libraries/synopsys $VHDLDIR/libraries/mentor
  mkdir $VHDLDIR/grt
  mkdir $VHDLDIR/grt/config
}

# Build Makefile
do_Makefile ()
{
  sed -e "/^####libraries Makefile.inc/r ../../libraries/Makefile.inc" \
      -e "/^####grt Makefile.inc/r ../grt/Makefile.inc" \
     < Makefile.in > $VHDLDIR/Makefile.in
  sed -e "/^####gcc Makefile.inc/r ../../ortho/gcc/Makefile.inc" \
     < Make-lang.in > $VHDLDIR/Make-lang.in
}

# Copy (or link) sources files into $VHDLDIR
do_files ()
{
# Local files
lfiles="config-lang.in lang-options.h lang-specs.h"
for i in $lfiles; do ln -sf $CWD/$i $VHDLDIR/$i; done

# ghdl core files
cfiles="
evaluation.adb
evaluation.ads
scan.ads
scan.adb
scan-scan_literal.adb
back_end.ads
back_end.adb
files_map.adb
files_map.ads
sem.adb
sem.ads
sem_expr.adb
sem_expr.ads
sem_names.adb
sem_names.ads
sem_scopes.adb
sem_scopes.ads
sem_decls.ads
sem_decls.adb
sem_specs.ads
sem_specs.adb
sem_stmts.ads
sem_stmts.adb
sem_types.ads
sem_types.adb
sem_assocs.ads
sem_assocs.adb
canon.adb
canon.ads
flags.adb
flags.ads
configuration.adb
configuration.ads
nodes.ads
nodes.adb
lists.ads
lists.adb
iirs.adb
iirs.ads
iir_chains.ads
iir_chains.adb
iir_chain_handling.ads
iir_chain_handling.adb
std_names.adb
std_names.ads
disp_tree.adb
disp_tree.ads
iirs_utils.adb
iirs_utils.ads
std_package.adb
std_package.ads
disp_vhdl.adb
disp_vhdl.ads
libraries.adb
libraries.ads
tokens.adb
tokens.ads
name_table.adb
name_table.ads
str_table.ads
str_table.adb
types.ads
version.ads
errorout.adb
errorout.ads
parse.adb
parse.ads
post_sems.ads
post_sems.adb
ieee.ads
ieee-std_logic_1164.ads
ieee-std_logic_1164.adb
ieee-vital_timing.ads
ieee-vital_timing.adb
xrefs.ads
xrefs.adb
bug.ads
bug.adb
"

for i in $cfiles; do ln -sf $CWD/../../$i $VHDLDIR/$i; done

ln -sf $CWD/../../doc/ghdl.texi $VHDLDIR/ghdl.texi

# translation file
tfiles="
translation.adb
ortho_front.adb
translation.ads
trans_decls.ads
trans_be.ads
trans_be.adb"

for i in $tfiles; do ln -sf $CWD/../$i $VHDLDIR/$i; done

ortho_files="
ortho_front.ads"

for i in $ortho_files; do ln -sf $CWD/../../ortho/$i $VHDLDIR/$i; done

ortho_gcc_files="
lang.opt
ortho-lang.c
ortho_gcc-main.adb
ortho_gcc-main.ads
ortho_gcc.adb
ortho_gcc.ads
ortho_gcc_front.ads
ortho_ident.adb
ortho_ident.ads
ortho_nodes.ads
"

for i in $ortho_gcc_files; do
  ln -sf $CWD/../../ortho/gcc/$i $VHDLDIR/$i
done

ghdl_files="
ghdl_gcc.adb
ghdldrv.ads
ghdldrv.adb
ghdlprint.ads
ghdlprint.adb
ghdllocal.ads
ghdllocal.adb
ghdlmain.ads
ghdlmain.adb
"

for i in $ghdl_files; do
  ln -sf $CWD/../ghdldrv/$i $VHDLDIR/ghdldrv/$i
done

libraries_files="
std/textio.vhdl
std/textio_body.vhdl
ieee/numeric_bit-body.vhdl
ieee/numeric_bit.vhdl
ieee/numeric_std-body.vhdl
ieee/numeric_std.vhdl
ieee/std_logic_1164.vhdl
ieee/std_logic_1164_body.vhdl
ieee/math_real.vhdl
ieee/math_real-body.vhdl
ieee/math_complex.vhdl
ieee/math_complex-body.vhdl
vital95/vital_primitives.vhdl
vital95/vital_primitives_body.vhdl
vital95/vital_timing.vhdl
vital95/vital_timing_body.vhdl
vital2000/memory_b.vhdl
vital2000/memory_p.vhdl
vital2000/prmtvs_b.vhdl
vital2000/prmtvs_p.vhdl
vital2000/timing_b.vhdl
vital2000/timing_p.vhdl
synopsys/std_logic_arith.vhdl
synopsys/std_logic_misc.vhdl
synopsys/std_logic_misc-body.vhdl
synopsys/std_logic_signed.vhdl
synopsys/std_logic_textio.vhdl
synopsys/std_logic_unsigned.vhdl
mentor/std_logic_arith.vhdl
mentor/std_logic_arith_body.vhdl
"

for i in $libraries_files; do
    echo "adding $i"
    ln -sf $CWD/../../libraries/$i $VHDLDIR/libraries/$i
done

grt_files="
grt-cbinding.c
grt-cvpi.c
grt.adc
grt-avhpi.adb
grt-avhpi.ads
grt-disp.adb
grt-disp.ads
grt-disp_rti.adb
grt-disp_rti.ads
grt-disp_signals.adb
grt-disp_signals.ads
grt-errors.adb
grt-errors.ads
grt-files.adb
grt-files.ads
grt-hooks.adb
grt-hooks.ads
grt-images.adb
grt-images.ads
grt-values.adb
grt-values.ads
grt-lib.adb
grt-lib.ads
grt-main.adb
grt-main.ads
grt-names.adb
grt-names.ads
grt-options.adb
grt-options.ads
grt-processes.adb
grt-processes.ads
grt-rtis.ads
grt-rtis_addr.adb
grt-rtis_addr.ads
grt-rtis_utils.adb
grt-rtis_utils.ads
grt-rtis_binding.ads
grt-rtis_types.ads
grt-rtis_types.adb
grt-sdf.adb
grt-sdf.ads
grt-shadow_ieee.ads
grt-shadow_ieee.adb
grt-signals.adb
grt-signals.ads
grt-stack2.adb
grt-stack2.ads
grt-stacks.adb
grt-stacks.ads
grt-c.ads
grt-zlib.ads
grt-stdio.ads
grt-astdio.ads
grt-astdio.adb
grt-types.ads
grt-vcd.adb
grt-vcd.ads
grt-vcdz.adb
grt-vcdz.ads
grt-vital_annotate.adb
grt-vital_annotate.ads
grt-vpi.adb
grt-vpi.ads
grt-vstrings.adb
grt-vstrings.ads
grt-stats.ads
grt-stats.adb
grt-waves.ads
grt-waves.adb
grt-avls.ads
grt-avls.adb
grt.ads
main.adb
main.ads
ghdl_main.ads
ghdl_main.adb
ghwlib.h
ghwlib.c
ghwdump.c
"

for i in $grt_files; do
    echo "adding $i"
    ln -sf $CWD/../grt/$i $VHDLDIR/grt/$i
done

grt_config_files="
i386.S
sparc.S
ppc.S
times.c
clock.c
linux.c
pthread.c
win32.c"

for i in $grt_config_files; do
    echo "adding $i"
    ln -sf $CWD/../grt/config/$i $VHDLDIR/grt/config/$i
done

}

# Create the tar of sources.
do_sources ()
{
    \rm -rf $distdir
    mkdir $distdir
    VHDLDIR=$distdir/vhdl
    do_clean $VHDLDIR
    do_Makefile
    do_files
    ln -sf ../../../COPYING $distdir
    sed -e "s/@GCCVERSION@/gcc-$GCCVERSION/g" < README > $distdir/README
    tar cvhf $tarfile $distdir
    bzip2 -f $tarfile
    rm -rf $distdir
}

# Put GHDL sources in GCC.
do_update_gcc_sources ()
{
  set -x

  cd $GCCDIST/..
  tar jxvf $CWD/$tarfile.bz2
  rm -rf $GCCDIST/gcc/vhdl
  mv $distdir/vhdl $GCCDIST/gcc
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
#  check_root;
  PATH=/usr/gnat/bin:$PATH
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
  rm -f ${DESTDIR}${PREFIX}/bin/i686-pc-linux-gnu-gcc*
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
put waveform.html
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
      Makefile|makefile)
	do_Makefile ;;
      files)
        do_files ;;
      sources)
        do_sources ;;
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
      *)
	echo "usage: $0 clean|Makefile|files|all"
	exit 1 ;;
     esac
   done
fi
