#! /bin/sh

. ../../testenv.sh

$PYTHON show_units.py

echo ""

$PYTHON show_ports.py ./

echo "Test successful"
