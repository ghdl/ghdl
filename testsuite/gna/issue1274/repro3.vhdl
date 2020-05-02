library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro3 is
end ;

architecture beh of repro3 is
begin
  process
    variable foo, bar : std_logic;
  begin
    (foo, bar) := "10"; -- crashes
    wait;
  end process;
end architecture;
