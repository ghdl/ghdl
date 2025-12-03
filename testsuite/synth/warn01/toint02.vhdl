library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity toint02 is
end;

architecture arch of toint02 is
  signal v : integer := to_integer(signed'("X01"));
begin
end;
