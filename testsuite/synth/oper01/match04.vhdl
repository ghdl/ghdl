library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity match04 is
  port (v : std_ulogic_vector(4 downto 0);
        v2 : std_logic;
        r : out boolean);
end match04;

architecture behav of match04 is
begin
  r <= std_match("1111" & v2, v); --  dyn
end behav;
