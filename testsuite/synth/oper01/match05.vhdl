library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity match05 is
  port (v : std_ulogic_vector(11 downto 0);
        r : out boolean);
end match05;

architecture behav of match05 is
begin
  r <= std_match("1111----00Z0", v);
end behav;
