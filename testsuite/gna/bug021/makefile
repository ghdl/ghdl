


all:


	rm -rf work
	mkdir work

		
	ghdl -a  --work=work --workdir=work sim_pkg.vhd
	gcc -c -fPIC sim.c -o sim.o
	
	
        
	ghdl -a  --work=work --workdir=work tb_cosim.vhd


	ghdl -e  -Wl,sim.o --ieee=synopsys -fexplicit --workdir=work -Pwork tb_cosim

#	ghdl -r tb_lm_cpu  
	ghdl -r tb_cosim  --wave=tbencha.ghw --stop-time=100ns
#	ghdl -r tbench --disp-tree=inst --wave=tbench.ghw 

#	ghdl -r tb_lm_cpu  


view:
	gtkwave tbencha.ghw a.sav


post:
	rm -rf work
	mkdir work
	ghdl -i ../ise601/netgen/synthesis/MAIS_soc_synthesis.vhd
	ghdl -i ../rtl/tb_MAIS.vhd
	ghdl -i --work=unisim /opt/Xilinx/14.5/ISE_DS/ISE/vhdl/src/unisims/*.vhd
	ghdl -i --work=unisim /opt/Xilinx/14.5/ISE_DS/ISE/vhdl/src/unisims/primitive/*.vhd



	ghdl -m  -g  -Punisim -Psimprim --warn-unused --ieee=synopsys -fexplicit  tb_lm_cpuvi
	ghdl -r tb_lm_cpu --disp-tree=inst --wave=tbench.ghw --stop-time=50us
