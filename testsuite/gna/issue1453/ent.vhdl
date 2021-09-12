entity ent is
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.bug.all;

architecture behav of ent is
  signal w : word;
  signal b : byte;
begin
  process
  begin
    w <= x"1234";
    wait for 1 ns;
    b <= high_byte(w);
    wait for 1 ns;
    assert b = x"12" severity failure;
    wait;
  end process;
end;
