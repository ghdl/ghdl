#! /bin/sh

. ../../testenv.sh

# Don't use grep -q, as SIGPIPE doesn't exist on windows.

$GHDL --file-to-xml t2.vhdl | grep "01X" > /dev/null
$GHDL --file-to-xml test.vhdl | grep '"00"' > /dev/null
clean

echo "Test successful"
