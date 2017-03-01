all:
	rm -rf work
	mkdir work
		
	ghdl -a  --work=work --workdir=work top_capitalisation.vhd	
	ghdl -a  --work=work --workdir=work capitalisation.vhd	
	ghdl -a  --work=work --workdir=work ../rx/UART_RX_8N1.vhd
	ghdl -a  --work=work --workdir=work ../tx/UART_TX_8N1.vhd
	ghdl -a  --work=work --workdir=work capitalisation.vhd
			
	ghdl -a  --work=work --workdir=work tb_capitalisation.vhd 
	ghdl -e  --workdir=work -Pwork tb_capitalisation 
	ghdl -r tb_capitalisation --wave=tbench.ghw --stop-time=700us 


view:
	gtkwave tbench.ghw a.gtkw

