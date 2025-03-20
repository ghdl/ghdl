library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_pkg.all;

entity rom1 is
  port (addr : std_logic_vector (3 downto 0);
        val : out std_logic_vector (31 downto 0));
end rom1;

architecture behav of rom1 is
begin
  val <= mem1 (to_integer(unsigned(addr)));
end behav;
