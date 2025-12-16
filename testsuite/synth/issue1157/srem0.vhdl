library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

entity srem0 is
  generic (zero : signed(0 to 1) := "00");
  port (o : out boolean);
end;

architecture behav of srem0 is
begin
  o <= signed'("010") rem zero = "00";
end;
