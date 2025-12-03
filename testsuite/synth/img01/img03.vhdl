library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity img03 is
end;

architecture arch of img03 is
begin
  process
    variable v : signed(4 downto 0);
  begin
    v := b"1_1000";
    assert to_hstring(v) = "F8" severity failure;
    wait;
  end process;
end;
