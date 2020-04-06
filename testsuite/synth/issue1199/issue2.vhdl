library IEEE;
use IEEE.std_logic_1164.all;

entity issue2 is
 port(
  a : in  std_logic_vector(7 downto 0);
  b : out std_logic_vector(2 downto 0)
 );
end issue2;

architecture behavior of issue2 is
begin
    b <= a;
end behavior;
