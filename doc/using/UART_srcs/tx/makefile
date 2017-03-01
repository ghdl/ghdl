
all:
	ghdl -i *.vhd
	ghdl -m tb_UART_TX_8N1
	ghdl -r tb_UART_TX_8N1 --stop-time=200us --wave=TX.ghw

view:
	gtkwave TX.ghw
