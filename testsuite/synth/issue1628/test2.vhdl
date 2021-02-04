library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity test2 is
  port (
   a : out std_logic_vector(3 downto 0));
end entity test2;

architecture synthesis of test2 is
begin
   a <= to_stdlogicvector(10, 4);
end architecture synthesis;

