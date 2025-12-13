library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity match03 is
  port (v : std_ulogic_vector(11 downto 0);
        r : out boolean);
end match03;

architecture behav of match03 is
begin
  r <= std_match("1111----0", v); --  Wrong length
end behav;
