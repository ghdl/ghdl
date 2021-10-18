entity uns02 is
  port (ok : out boolean);
end uns02;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of uns02 is
  --  add uns uns
  constant a : unsigned (7 downto 0) := x"1e";
  constant b : unsigned (3 downto 0) := x"2";
  constant r1 : unsigned (7 downto 0) := a - b;
  signal er1 : unsigned (7 downto 0);
  signal ok1 : boolean;
  constant cok1 : boolean := and(a) = '0';
  constant cok2 : boolean := or(a) = '1';
begin
  assert cok1 and cok2 severity failure;
  er1 <= x"1c";
  ok1 <= and(er1) = '0';
  ok <= ok1;
end behav;
