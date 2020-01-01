library ieee;
use ieee.std_logic_1164.all;

entity matching is
end matching;

architecture behav of matching is
  constant ceq : std_logic := '1' ?= '1';
begin
  assert ceq = '1';
end behav;
