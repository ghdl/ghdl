library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity conv01 is
end;

architecture arch of conv01 is
  constant v0 : integer := conv_integer(std_logic'('0'));
  constant v1 : integer := conv_integer(std_logic'('1'));
begin
  assert v0 = 0 and v1 = 1;
end;
