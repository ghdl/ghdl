library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity shift1 is
  port (n : natural;
        q : out std_logic_vector(31 downto 0));
end;

architecture behav of shift1 is
begin
  q <= std_logic_vector(to_unsigned(2 ** n, 32));
end;
