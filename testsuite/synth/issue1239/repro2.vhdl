library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
  generic ( 
    constant DIN_WIDTH      : positive := 8;	
    constant F_SIZE         : positive := 2
    ); 
end repro2;

architecture Behav of repro2 is
  type SLIDING_WINDOW is array (0 to F_SIZE-1, 0 to F_SIZE-1)
    of STD_LOGIC_VECTOR(DIN_WIDTH- 1 downto 0);
  signal WINDOW: SLIDING_WINDOW;
begin
   WINDOW <=(WINDOW 'range=> (WINDOW 'range=> (WINDOW 'range=>'0')));
end Behav;

