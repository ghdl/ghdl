library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity img06 is
end;

architecture arch of img06 is
begin
  process
    variable s0 : signed(1 to 0);
  begin
    assert to_hstring(s0) = "" severity failure;
    wait;
  end process;
end;
