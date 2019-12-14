entity snum01 is
  port (ok : out boolean);
end snum01;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of snum01 is
  --  add uns uns
  constant a : unsigned (7 downto 0) := x"1e";
  constant b : unsigned (3 downto 0) := x"2";
  constant r1 : unsigned (7 downto 0) := a + b;
  signal er1 : unsigned (7 downto 0);
begin
  er1 <= x"20";
--  ok <= r1 = x"20";
  ok <= r1 = er1;
end behav;
