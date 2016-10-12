#! /bin/sh

. ../../testenv.sh

analyze qualified_expr.vhdl
elab_simulate qualified_expr
clean

echo "Test successful"
