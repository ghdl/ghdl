library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity test is
end entity test;

architecture synthesis of test is

   signal a : std_logic_vector(3 downto 0);

begin

   a <= to_stdlogicvector(10, 4);

end architecture synthesis;

