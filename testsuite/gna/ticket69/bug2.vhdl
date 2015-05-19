library ieee;
use ieee.numeric_std.all;

entity ent is
end entity;

architecture a of ent is
begin
  main : process
    variable a,b : unsigned(0 downto 0) := "1";
  begin
    assert a = b; -- Works
    assert ieee.numeric_std."="(a, b);
  end process;
end architecture;
