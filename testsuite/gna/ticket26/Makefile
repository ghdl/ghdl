psl_test_cover: psl_test_named_statement.vhd
	ghdl -a --std=02 -fpsl psl_test_named_statement.vhd
	ghdl -e --std=02 -fpsl psl_test_named_statement
	./psl_test_named_statement --stop-time=200ns

all: clean psl_test_named_statement

.PHONY: clean
clean:
	rm -f *.cf
	rm -f *.o
	rm -f psl_test_named_statement