library ieee;
use ieee.std_logic_1164.all;

entity file1 is 
  port(
    test_out : out std_logic_vector(15 downto 0)
  );
end entity;

architecture behavioral of file1 is 

begin 
  test_out <= x"ABCD";
end architecture;
