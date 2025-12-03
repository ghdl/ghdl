library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity toint03 is
end;

architecture arch of toint03 is
  signal v : integer := to_integer(signed'("0X1"));
begin
end;
