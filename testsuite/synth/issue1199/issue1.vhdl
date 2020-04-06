library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity issue1 is
 port(
  a : in  std_logic_vector(7 downto 0);
  b : out std_logic_vector(2 downto 0)
 );
end issue1;

architecture behavior of issue1 is
begin
    b <= std_logic_vector(unsigned(a) + 1);
end behavior;
