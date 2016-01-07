library ieee;
use ieee.std_logic_1164.all;

entity repro is
end repro;

architecture behav of repro is
begin
  process
    variable z : std_ulogic_vector (1 to 2) := "00";
    variable r : std_ulogic;
  begin
    r := z ?= "LL";
    assert r = '1';
    z := "--";
    r := z ?= "00";  -- One warning
    r := z ?= "--";  -- Two warning
    wait;
  end process;
end behav;
