library ieee;
use ieee.std_logic_1164.all;

entity nand01 is
  port (l : std_logic_vector(3 downto 0);
        r :  std_logic_vector(3 downto 0);
        o :  out std_logic_vector(3 downto 0));
end nand01;

architecture behav of nand01 is
begin
  o <= l nand r;
end behav;
