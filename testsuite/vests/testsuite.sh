#! /bin/sh

. ../testenv.sh

common_args="--std=93c -Werror=runtime-error $GHDL_FLAGS"
# Test number.
test_num="1"
do_inter_clean="no"
use_last_entity=no
dry=false

do_run()
{
    echo $*
    if [ "$dry" != true ]; then
	eval $*
    fi
}

# Functions used by tests.
setup_test_group() { echo "Test: $1 $2"; }
end_test_group() { delete_lib work; echo "*** End of tests"; }
create_lib() { echo "create library: $1"; }
delete_lib() { echo "delete library: $1" && cmd="$GHDL --remove $common_args --work=$1" && do_run $cmd; }

get_entity()
{
    if [ x$use_last_entity = x"no" ]; then
	$GHDL --find-top $1
    else
	# Search for XXXent or XXXentw, keep only the last one
	sed -n -E -e '/^ENTITY .*(ent|entw|cfg) IS$/s/ENTITY (.*) IS/\1/p' \
	    $1 | tail -1
    fi
}

# Usage: handle_test MODE FILE options...
handle_test() {
  mode=$1
  shift
  file=$1
  shift
  args="$common_args"
  stop=""
  entity=""
  # handle options.
  for arg; do
    case $arg in
    LIBRARY=*)
         lib=`echo $arg | sed -e s/LIBRARY=/--work=/`;
         args="$args $lib";
         ;;
    INPUT=*)
         input=$arg;
         ;;
    OUTPUT=*)
         output=$arg;
         ;;
    STOP=*)
         stop=`echo $arg | sed -e s/STOP=/--stop-time=/`;
         ;;
    ENTITY=*)
         entity=`echo $arg | sed -e s/ENTITY=//`
	 ;;
    *)
         echo "build_compliant_test: unknown argument '$arg'";
         exit 4;
         ;;
    esac
  done
  cmd="$GHDL -a $args $dir/$file"
  echo "Test: $test_num"

  if [ $test_num -gt $skip ]; then
    case $mode in
    compile)
        do_run $cmd;
        ;;
    run)
        do_run $cmd
	if [ $dry = true ]; then
	    return
	fi
        if [ "x$entity" = "x" ]; then
            entity=`get_entity $dir/$file`
	fi
        if [ "x$entity" = "x" ]; then
            echo "Cannot elaborate or run : no top level entity";
	    exit 1
        else
            cmd="$GHDL --elab-run $entity $stop --assert-level=error";
            do_run "$cmd";
        fi
        ;;
    ana_err)
        if do_run $cmd; then
	    if [ $dry = true ]; then
		return;
	    fi
            echo "Analyze error expected";
            exit 1;
        fi
        ;;
    run_err)
        do_run $cmd
#         ent=`sed -n -e "/^ENTITY \([a-zA-Z0-9]*\) IS$/p" < $dir/$file \
#              | cut -f 2 -d ' '`
        if [ x$entity = "x" ]; then
            entity=`get_entity $dir/$file`
	fi
        if [ "x$entity" = "x" ]; then
            echo "Cannot elaborate or run : no top level entity";
            exit 1;
        else
            cmd="$GHDL -e $entity";
            do_run "$cmd";
            cmd="$GHDL -r $entity $stop --expect-failure --assert-level=error";
            do_run "$cmd";
        fi
        ;;
    *)
        echo "Unknown mode '$mode'";
        exit 4;
        ;;
    esac

    if [ $do_inter_clean = "yes" ]; then
#      if [ `expr $test_num % 16` = "0" ]; then
         delete_lib work;
#      fi
    fi
  else
    echo "skip";
  fi

  # Increment test_num
  test_num=`expr $test_num + 1`
}

build_compliant_test () { handle_test compile $@; }
run_non_compliant_test () { handle_test ana_err $@; }
run_compliant_test () { handle_test run $@; }
run_err_non_compliant_test () { handle_test run_err $@; }

# Decode options.
skip=0

while [ $# -gt 0 ]
do
  arg=$1;
  case $arg in
    -j)  shift;
         skip=$1;
         ;;
    -n)  dry=true
	 ;;
    *) exit 1;
         ;;
  esac
  shift;
done

# Test groups

test_ashenden() {
  delete_lib work
  use_last_entity=no
  do_inter_clean="yes"

  dir=vhdl-93/clifton-labs/compliant
  . $dir/compliant1.exp

  # ashenden compliant
  do_inter_clean="no"
  # OK
  dir=vhdl-93/ashenden/compliant
  . $dir/compliant.exp

  # OK
  dir=vhdl-93/ashenden/non_compliant
  . $dir/non_compliant.exp
}

test_billowitch() {
  use_last_entity=yes

  # OK.
  dir=vhdl-93/billowitch/compliant
  . $dir/compliant.exp

  # OK but FIXMEs
  common_args="--std=93 -Werror=runtime-error $GHDL_FLAGS"

  dir=vhdl-93/billowitch/non_compliant/analyzer_failure
  . $dir/non_compliant.exp

  run_non_compliant_test() { handle_test run_err $@; }

  dir=vhdl-93/billowitch/non_compliant/simulator_failure
  . $dir/non_compliant.exp
}

deletelibs() {
  delete_lib project
  delete_lib random
  delete_lib utilities
}

printf "$ANSI_BLUE[GHDL - test] vests: ashenden $ANSI_NOCOLOR\n"
test_ashenden
# Clean frequently the work library.
do_inter_clean="yes"
printf "$ANSI_BLUE[GHDL - test] vests: billowitch $ANSI_NOCOLOR\n"
test_billowitch
printf "$ANSI_BLUE[GHDL - test] vests: delete libs $ANSI_NOCOLOR\n"
deletelibs

# Remove io files created by tests
rm -f iofile.* *.file fopen*.out

echo "Vests tests successful"
