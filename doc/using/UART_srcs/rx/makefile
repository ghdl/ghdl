
all:
	ghdl -i *.vhd
	ghdl -m tb_UART_RX_8N1
	ghdl -r tb_UART_RX_8N1 --stop-time=800us --wave=RX.ghw

view:	
	gtkwave RX.ghw
