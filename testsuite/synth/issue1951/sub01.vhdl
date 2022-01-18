library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity sub01 is
  port (i : std_logic_vector (3 downto 0);
        o : out std_logic_vector (3 downto 0));
end entity;

architecture arch of sub01 is
begin
  o <= i - (-1);
end arch;
