library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
end repro1;

architecture behav of repro1 is
begin
  assert std_ulogic_vector'("LL") ?= "00";
end behav;
