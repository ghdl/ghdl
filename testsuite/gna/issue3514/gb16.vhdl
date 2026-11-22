library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity gb16 is end entity;

architecture a of gb16 is
  type ram_t is record
    adr : unsigned;
  end record;
  type top_t is record
    r : ram_t(adr(7 downto 0));
    s : signed(15 downto 0);
  end record;
  signal c : top_t;
begin
  c.s <= x"003c"; -- to_signed(60, 16);
  process begin
    wait for 1 ns;
    report "RESULT: s=" & to_hstring(c.s) & " (expected 003C)";
    assert c.s = x"003c" severity failure;
    wait;
  end process;
end architecture;
