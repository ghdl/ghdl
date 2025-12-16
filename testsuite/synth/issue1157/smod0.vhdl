library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

entity smod0 is
  generic (zero : signed(0 to 1) := "00");
  port (o : out boolean);
end;

architecture behav of smod0 is
begin
  o <= signed'("010") mod zero = "00";
end;
