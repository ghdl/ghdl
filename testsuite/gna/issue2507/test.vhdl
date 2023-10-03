library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;

architecture beh of test is
begin
  
  process
  begin
    assert signed'("11") = to_signed(-1, 2) report "to_signed" severity failure;
    wait;
  end process;

end architecture beh;
