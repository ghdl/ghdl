library ieee;
use ieee.std_logic_1164.all;

entity repro3 is
  generic ( 
    constant DIN_WIDTH      : positive := 8;
    constant FIFO_DEPTH     : positive := 12
    ); 
end repro3;

architecture Behav of repro3 is
  type       FIFO_Memory             is array (0 to FIFO_DEPTH - 1)          of STD_LOGIC_VECTOR(DIN_WIDTH - 1 downto 0);
  
  signal FIFO_ROW_1  	: FIFO_Memory;
begin
  FIFO_ROW_1<= (FIFO_ROW_1 'range=> (FIFO_ROW_1 'range=>'0'));
end Behav;

