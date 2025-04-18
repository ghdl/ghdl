library ieee;
use ieee.std_logic_1164.all;
use work.repro2_pkg.all;

entity repro2 is
  port (a : in std_logic;
        b_rst : std_logic;
        b : out my_rec);
end;

architecture behav of repro2 is
begin
  b.rst <= b_rst;
end behav;
