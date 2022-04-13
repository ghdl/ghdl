
ENTITY cdc_fifo IS
END ENTITY cdc_fifo;

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ARCHITECTURE rtl OF cdc_fifo IS
  SIGNAL ver_clk : std_logic;
  ATTRIBUTE gclk : boolean;
  ATTRIBUTE gclk OF ver_clk : SIGNAL is true;
BEGIN
END ARCHITECTURE;
