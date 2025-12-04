library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity img07 is
end;

architecture arch of img07 is
begin
  process
    variable v0 : std_logic_vector(1 to 0);
  begin
    assert to_ostring(v0) = "" severity failure;
    wait;
  end process;
end;
