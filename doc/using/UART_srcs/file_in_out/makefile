all:
	rm -rf work
	mkdir work
		
	ghdl -a  --work=work --workdir=work ../capitalisation/capitalisation.vhd	
	ghdl -a  --work=work --workdir=work tb_file.vhd 
	ghdl -e    --ieee=synopsys -fexplicit --workdir=work -Pwork tb_file 
	ghdl -r tb_file  --wave=tbench.ghw  --stop-time=200us


view:
	gtkwave tbench.ghw a.gtkw

