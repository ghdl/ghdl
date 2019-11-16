entity snum03 is
  port (ok : out boolean);
end snum03;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of snum03 is
  --  add uns nat
  constant a1 : unsigned (7 downto 0) := x"1d";
  constant b1 : integer := 3;
  constant r1 : unsigned (7 downto 0) := a1 + b1;
  
  signal er1 : unsigned (7 downto 0) := x"20";
begin
--  ok <= r1 = x"20";
  ok <= r1 = er1;
end behav;
