library ieee;
use ieee.std_logic_1164.all;

entity repro is
end repro;

architecture behav of repro is
begin
  assert std_ulogic_vector'("LL") ?= "00";
end behav;
