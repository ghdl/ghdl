library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro3 is
  port (n : natural;
        d1 : std_logic_vector(15 downto 0);
        q : out std_logic_vector(31 downto 0));
end;

architecture behav of repro3 is
begin
  q <= std_logic_vector
       (resize(unsigned(d1), 32) and to_unsigned(3 ** n-1, 32));
end;
