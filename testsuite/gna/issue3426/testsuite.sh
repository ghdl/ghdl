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

# 1076-2008 15.10 does not reserve inherit.  The words reserved by 1076-2008
# remain accepted as identifiers in 2008, for backward compatibility.
mk inherit
GHDL_STD_FLAGS=--std=08 analyze w_inherit.vhdl
for w in fairness strong assume_guarantee; do
  mk $w
  GHDL_STD_FLAGS=--std=08 analyze w_$w.vhdl
done
mk restrict_guarantee
GHDL_STD_FLAGS=--std=08 analyze_failure w_restrict_guarantee.vhdl

# Older standards still accept the 2019 words, without a warning by default.
for std in 93 02 08; do
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

# -Wreserved-word warns about the use of future reserved words.
for w in fairness strong vpkg; do
  mk $w
  "$GHDL" -a --std=93 -Wreserved-word w_$w.vhdl 2> log.txt
  grep "using \"$w\" vhdl-20.. reserved word as an identifier" log.txt
  "$GHDL" -a --std=93 -Wreserved-word -Wno-reserved-word w_$w.vhdl 2> log.txt
  if test -s log.txt; then
    echo "-Wno-reserved-word did not disable the warning for $w"
    exit 1
  fi
done

# inherit remains usable in a verification unit.
GHDL_STD_FLAGS=--std=08 analyze vunit.vhdl
GHDL_STD_FLAGS=--std=19 analyze vunit.vhdl

clean
rm -f work-obj*.cf w_*.vhdl log.txt

echo "Test successful"
