library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
  port (a : unsigned (0 to 0);
        o : out boolean);
end;

architecture behav of repro is
begin
  o <= a <= "0";
end behav;
