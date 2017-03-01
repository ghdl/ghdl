


all:

	rm -rf work
	mkdir work
	
	ghdl -a  --work=work --workdir=work tty_pkg.vhd
	gcc -c -fPIC tty.c  -o tty.o

	ghdl -a  --work=work --workdir=work ../capitalisation/capitalisation.vhd	
	ghdl -a  --work=work --workdir=work tb_tty.vhd 
		       
	ghdl -e -Wl,tty.o  --ieee=synopsys -fexplicit --workdir=work -Pwork tb_tty 
#	ghdl -r tb_tty  --wave=tbench.ghw  
	ghdl -r tb_tty  --wave=tbench.ghw  --stop-time=500000us


view:
	gtkwave tbench.ghw a.gtkw

