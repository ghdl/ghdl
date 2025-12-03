library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity toint01 is
end;

architecture arch of toint01 is
  constant v0 : integer := conv_integer(std_logic'('0'));
  constant v1 : integer := conv_integer(std_logic'('1'));
begin
  assert v0 = 0 and v1 = 1;
end;
