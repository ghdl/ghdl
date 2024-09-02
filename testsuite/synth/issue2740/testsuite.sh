#! /bin/sh

. ../../testenv.sh

! synth --out=verilog --std=08 --no-formal -fsynopsys \
  -gCLK_FREQ=4000000.0 -gBAUD_RATE=9600 -gPARITY_BIT=none -gUSE_DEBOUNCER=0 \
  uart.vhd uart_clk_div.vhd uart_debouncer.vhd \
  uart_parity.vhd uart_rx.vhd uart_tx.vhd -e UART 2> synth.log
grep -q "override for generic" synth.log
! grep Bug synth.log

synth --out=verilog --std=08 --no-formal -fsynopsys \
  -gCLK_FREQ=4000000 -gBAUD_RATE=9600 -gPARITY_BIT=none -gUSE_DEBOUNCER=False \
  uart.vhd uart_clk_div.vhd uart_debouncer.vhd \
  uart_parity.vhd uart_rx.vhd uart_tx.vhd -e UART > synth_uart.v

echo "Test successful"
