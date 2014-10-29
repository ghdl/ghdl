.PHONY: default
default:
	-make works.o
	-make fails1.o
	-make fails2.o


%.o: %.vhdl
	ghdl -a $<

.PHONY: clean
clean:
	ghdl --clean
	rm *.cf

