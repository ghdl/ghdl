#! /bin/sh

if [ "$OS" = "Windows_NT" ]; then
    # Many issues with blanks in command line.  Not worth fixing it.
    echo "Test skipped on windows"
    exit 0
fi

. ../../testenv.sh

# Incorrect options are always rejected (analyze_failure doesn't work here)

# No identifier
! $GHDL -a --work= p.vhdl

# No extended identifiers in v87
! $GHDL -a --std=87 '--work=\wrk\' p.vhdl

# Empty extended identifier
! $GHDL -a '--work=\\' p.vhdl

# Missing ending \
! $GHDL -a --work=\\wrk p.vhdl

# Ok
$GHDL -a "--work=\\wrk \\" p.vhdl
$GHDL --remove "--work=\\wrk \\"

# FIXME
# $GHDL -a '--work=\wr\\k \' p.vhdl

# Invalid char in extended identifier
! $GHDL -a '--work=\wrk\' p.vhdl
! $GHDL -a '--work=\wrk\' p.vhdl

# Non doubled \ in extended identifier
! $GHDL -a '--work=\wr\k\' p.vhdl

# Incorrect doubled \ in extended identifier
! $GHDL -a '--work=\wrk\\' p.vhdl

# Incorrect char in identifier (v87)
! $GHDL -a --std=87 '--work=wåk' p.vhdl
! $GHDL -a --std=87 '--work=wÅk' p.vhdl

# Incorrect char in indentifier
! $GHDL -a --work="wrk" p.vhdl
! $GHDL -a --work="wrk/" p.vhdl

# Incorrect use of _
! $GHDL -a --work=_wrk p.vhdl
! $GHDL -a --work=wrk_ p.vhdl
! $GHDL -a --work=wr__k p.vhdl


# File with various end of lines
analyze allnl.vhdl

clean

echo "Test successful"
