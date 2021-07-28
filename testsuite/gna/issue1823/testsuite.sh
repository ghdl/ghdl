#! /bin/sh

. ../../testenv.sh

for f in entity_1.vhdl entity2.vhdl entity3.vhdl; do
    echo "parsing $f"
    if $GHDL -i $f; then
        echo "error expected during parse"
    fi
done

clean

echo "Test successful"
