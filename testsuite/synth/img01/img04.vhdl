library ieee;
use ieee.std_logic_1164.all;

entity img04 is
end;

architecture arch of img04 is
begin
  process
    variable v : time := 4 ns;
  begin
    assert to_string(v, 2 ns) = "2" severity failure;
    wait;
  end process;
end;
