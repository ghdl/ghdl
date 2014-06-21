psl_test_cover: psl_test_cover.vhd
	ghdl -a --std=02 -fpsl psl_test_cover.vhd
	ghdl -e --std=02 -fpsl psl_test_cover
	./psl_test_cover --stop-time=200ns

all: clean psl_test_cover

.PHONY: clean
clean:
	rm -f *.cf
	rm -f *.o
	rm -f psl_test_cover