library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity flip_flop is 
  port (
         clk : in std_logic;
         wire : in std_logic;
         reg : out std_logic
       );
end;

architecture a_flip_flop of flip_flop is 
begin
  reg <= wire when rising_edge(clk);
end;
