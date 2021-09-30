library ieee;
use ieee.std_logic_1164.all;

entity repro is
end;

architecture behav of repro is
  signal s : std_logic := '0';
begin
  process
  begin
    for v in std_logic loop
      s <= v;
      wait for 1 ns;
    end loop;
    wait;
  end process;
end behav;
