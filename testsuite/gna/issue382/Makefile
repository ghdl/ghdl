ANALYSE=ghdl -a
ELABORATE=ghdl -e
VPATH=.

OBJECTS= demo.o tb_demo.o

all: tb_demo

sim:
	./tb_demo --stop-time=15sec --wave=tb_demo.ghw
gsim:
	killall gtkwave; true
	./tb_demo --stop-time=300sec --wave=tb_demo.ghw
	gtkwave tb_demo.ghw tb_demo.gtk
                        
tb_demo: $(OBJECTS)
	$(ELABORATE) $@

%.o:  %.vhd
	$(ANALYSE) $<

clean:
	$(RM) *.o tb_demo e~tb_demoe.lst *~ work-obj93.cf

