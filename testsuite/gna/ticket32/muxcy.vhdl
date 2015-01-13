library ieee;
use ieee.std_logic_1164.all;

entity muxcy is
  port (s, di, ci : std_logic;
        o : out std_logic);
end muxcy;

architecture behav of muxcy is
begin
  o <= di when s = '0' else ci;
end behav;
