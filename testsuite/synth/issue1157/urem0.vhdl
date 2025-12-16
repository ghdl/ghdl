library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

entity urem0 is
  generic (zero : unsigned(0 to 1) := "00");
  port (o : out boolean);
end;

architecture behav of urem0 is
begin
  o <= unsigned'("010") rem zero = "00";
end;
