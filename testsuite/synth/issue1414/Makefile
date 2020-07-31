GHDL=ghdl
GHDLFLAGS=--ieee=synopsys -O2

MODULES= \
	test.vhd \

.PHONY:
all: elaboration.done
	./tb --ieee-asserts=disable --dump-rti

elaboration.done: analyse.done
	$(GHDL) -e $(GHDLFLAGS) -o tb tb
	touch $@

analyse.done: $(MODULES)
	echo Building rofl...
	$(GHDL) -a $(GHDLFLAGS) $^
	touch $@

.PHONY:
clean:
	echo "Cleaning up..."
	rm -f *.o *.cf *.done *.ghw tb
