library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue is
    port (foo    : out unsigned (1 downto 0);
          bar    : out unsigned (2 downto 0);
          foobar : out unsigned (4 downto 0));
end entity issue;

architecture beh of issue is
begin
    assert 5ub"11_000" = unsigned'(b"11_000");
    foobar <= 5ub"11_000" + 5ub"00_000"; -- works
    (foo, bar) <= 5ub"11_000" + 5ub"00_000"; -- crashes
end architecture;
