library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity toint05 is
end;

architecture arch of toint05 is
  signal v : integer := conv_integer(std_logic'('X'));
begin
end;
