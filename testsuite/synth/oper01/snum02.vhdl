entity snum02 is
  port (ok : out boolean);
end snum02;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of snum02 is
  --  add sgn int
  constant a1 : signed (7 downto 0) := x"1d";
  constant b1 : integer := 3;
  constant r1 : signed (7 downto 0) := a1 + b1;
  
  constant a2 : signed (7 downto 0) := x"24";
  constant b2 : integer := -4;
  constant r2 : signed (7 downto 0) := a2 + b2;
  
  signal er1 : signed (7 downto 0) := x"20";
begin
--  ok <= r1 = x"20";
  ok <= r1 = er1 and r2 = er1;
end behav;
