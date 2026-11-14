#! /bin/sh

. ../../testenv.sh

# Create a design that uses $1 as the name of a signal.
mk ()
{
  cat > w_$1.vhdl <<EOF
entity w_$1 is end entity;
architecture a of w_$1 is
  signal $1 : bit;
begin
end architecture;
EOF
}

# 1076-2019 15.10: these are reserved words.
for w in fairness strong vpkg private view; do
  mk $w
  GHDL_STD_FLAGS=--std=19 analyze_failure w_$w.vhdl
done

# 1076-2019 15.10: these are not reserved words.
for w in inherit restrict_guarantee assume_guarantee; do
  mk $w
  GHDL_STD_FLAGS=--std=19 analyze w_$w.vhdl
done

# 1076-2008 15.10 reserves fairness, strong and restrict_guarantee but
# not inherit.
mk inherit
GHDL_STD_FLAGS=--std=08 analyze w_inherit.vhdl
for w in fairness strong restrict_guarantee; do
  mk $w
  GHDL_STD_FLAGS=--std=08 analyze_failure w_$w.vhdl
done
mk assume_guarantee
GHDL_STD_FLAGS=--std=08 analyze w_assume_guarantee.vhdl

# Older standards still accept the newer words, without a warning by default.
for std in 93 02; do
  for w in fairness strong vpkg; do
    mk $w
    "$GHDL" -a --std=$std w_$w.vhdl 2> log.txt
    if test -s log.txt; then
      echo "unexpected output for $w (--std=$std)"
      cat log.txt
      exit 1
    fi
  done
done
mk vpkg
"$GHDL" -a --std=08 w_vpkg.vhdl 2> log.txt
if test -s log.txt; then
  echo "unexpected output for vpkg (--std=08)"
  cat log.txt
  exit 1
fi

# -Wreserved-word warns about the use of future reserved words.
for w in fairness strong vpkg; do
  case $w in vpkg) v=2019;; *) v=2008;; esac
  mk $w
  "$GHDL" -a --std=93 -Wreserved-word w_$w.vhdl 2> log.txt
  grep "using \"$w\" vhdl-$v reserved word as an identifier" log.txt
  "$GHDL" -a --std=93 -Wreserved-word -Wno-reserved-word w_$w.vhdl 2> log.txt
  if test -s log.txt; then
    echo "-Wno-reserved-word did not disable the warning for $w"
    exit 1
  fi
done

# inherit remains usable in a verification unit (syntax check only: the
# code generator does not handle vunit).
"$GHDL" -s --std=08 vunit.vhdl
"$GHDL" -s --std=19 vunit.vhdl

clean
rm -f work-obj*.cf w_*.vhdl log.txt

echo "Test successful"
