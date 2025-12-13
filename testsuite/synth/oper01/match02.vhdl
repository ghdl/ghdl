library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity match02 is
  port (v : std_ulogic_vector(11 downto 0);
        r : out boolean);
end match02;

architecture behav of match02 is
begin
  r <= std_match("1111----0000", v);
end behav;
