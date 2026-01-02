library ieee;
use ieee.std_logic_1164.all;

entity const02 is
  port (o : out std_logic_vector(47 downto 0));
end;

architecture behav of const02 is
begin
  o <= "0001001101110ZX10001001101110ZX10001001101110ZX1";
end;
