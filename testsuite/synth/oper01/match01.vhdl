library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity match01 is
  port (v : std_ulogic_vector(11 downto 0);
        r : out boolean);
end match01;

architecture behav of match01 is
begin
  r <= std_match(v, "1111----0000");
end behav;
