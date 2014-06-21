psl_test_working: psl_test_working.vhd
	ghdl -a --std=02 -fpsl psl_test_working.vhd
	ghdl -e --std=02 -fpsl psl_test_working
	./psl_test_working --stop-time=200ns

psl_test_error: psl_test_error.vhd
	ghdl -a --std=02 -fpsl psl_test_error.vhd
	ghdl -e --std=02 -fpsl psl_test_error
	./psl_test_error --stop-time=200ns

all: clean psl_test_working psl_test_error

.PHONY: clean
clean:
	rm -f *.cf
	rm -f *.o
	rm -f psl_test_working
	rm -f psl_test_error