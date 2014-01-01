library IEEE;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

-- A testbench has no ports.
entity system is
end system;

architecture behav of system is
  subtype entry is unsigned(7 downto 0);
  type invect is array (natural range <>) of entry;
  signal minimum : entry;
  signal vec : invect(0 to 20);
  function min(iv : invect) return entry is
  begin
    return iv(0);
  end;
begin
  process
  begin
    minimum <= min(invect); -- should be vec not invect
    wait;
  end process;
end behav;
