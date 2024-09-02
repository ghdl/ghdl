$GHDL --synth --out=verilog --std=08 --no-formal -fsynopsys \
      -gCLK_FREQ=4000000.0 -gBAUD_RATE=9600 -gPARITY_BIT=none -gUSE_DEBOUNCER=0 \
      rtl/uart.vhd rtl/comp/uart_clk_div.vhd rtl/comp/uart_debouncer.vhd rtl/comp/uart_parity.vhd rtl/comp/uart_rx.vhd rtl/comp/uart_tx.vhd -e UART
