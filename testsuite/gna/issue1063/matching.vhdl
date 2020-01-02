library ieee;
use ieee.std_logic_1164.all;

entity matching is
end matching;

architecture behav of matching is
  constant ceq11 : std_logic := '1' ?= '1';
  constant ceq1h : std_logic := '1' ?= 'H';
  constant ceq1w : std_logic := '1' ?= 'W';
begin
  assert ceq11 = '1';
  assert ceq1h = '1';
  assert ceq1w = 'X';
end behav;
