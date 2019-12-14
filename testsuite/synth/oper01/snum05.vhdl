entity snum05 is
  port (ok : out boolean);
end snum05;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of snum05 is
  --  add uns nat
  constant a1 : signed (7 downto 0) := x"1d";
  constant b1 : signed (3 downto 0) := x"5";
  constant r1 : signed (11 downto 0) := a1 * b1;
  
  signal er1 : signed (11 downto 0) := x"091";
  
  constant a2 : signed (7 downto 0) := x"fe";
  constant b2 : signed (3 downto 0) := x"f";
  constant r2 : signed (11 downto 0) := a2 * b2;
  signal er2 : signed (11 downto 0) := x"002";
  
begin
--  ok <= r1 = x"20";
  ok <= r1 = er1 and r2 = er2;
end behav;
