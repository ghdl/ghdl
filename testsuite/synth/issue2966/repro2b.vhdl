library ieee;
use ieee.std_logic_1164.all;
use work.repro2_pkg.all;

entity repro2b is
  port (a : out std_logic;
        b_rst : out std_logic;
        b : in my_rec);
end;

architecture behav of repro2b is
begin
  b_rst <= b.rst;
  a <= b.rdy;
end behav;
