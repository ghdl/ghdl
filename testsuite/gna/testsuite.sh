#! /bin/sh

# Driver for the GNA testsuite.
# Each test correspond to one report, and is put in one directory (using
# the support/bug number)
#
# bug0XX is for bugs not reported.
# bug[1-9]XXX is for bugs reported on https://gna.org/bugs/?group=ghdl
# srXXX is for support reported on https://gna.org/support/?group=ghdl
# debXX is for bugs report on https://www.debian.org/Bugs/
# ticketXX is from https://sourceforge.net/p/ghdl-updates/tickets/
# issueXXX is from https://github.com/tgingold/ghdl/issues

set -e

dirs="bug* sr* deb* ticket* issue*"
failures=""
full=n

for opt; do
  case "$opt" in
  -k | --keep-going)  full=y ;;
  --dir=*) dirs=`echo $opt | sed -e 's/--dir=//'` ;;
  --skip=*) d=`echo $opt | sed -e 's/--skip=//'`
            dirs=`echo "" $dirs | sed -e "s/ $d//"` ;;
  --start-at=*) d=`echo $opt | sed -e 's/--start-at=//'`
            dirs=`echo "" $dirs | sed -e "s/^.* $d//"`
            dirs="$d $dirs" ;;
  *) echo "Unknown option $opt"
     exit 2
     ;;
  esac
done

for i in $dirs; do
  echo "GNA dir $i:"
  cd $i
  if ! ./testsuite.sh; then
    echo "#################################################################"
    echo "######### FAILURE: $i"
    echo "#################################################################"
    if [ $full = "y" ]; then
      failures="$failures $i"
    else
      exit 1;
    fi
  fi
  cd ..
done

if [ x"$failures" = x"" ]; then
    echo "GNA tests are successful"
    exit 0
else
    echo "GNA test failed ($failures)"
    exit 1
fi

