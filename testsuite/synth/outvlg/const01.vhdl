library ieee;
use ieee.std_logic_1164.all;

entity const01 is
  port (o : out std_logic_vector(15 downto 0));
end;

architecture behav of const01 is
begin
  o <= "0001001101110ZX1";
end;
