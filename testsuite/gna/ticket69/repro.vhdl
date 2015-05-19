library ieee;
use ieee.numeric_std.all;

entity ent is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture a of ent is
begin
  main : process
    variable a,b : unsigned(0 downto 0) := "1";
  begin
    assert a = b; -- Works
    assert ieee.numeric_std."="(a, b);
    wait;
  end process;
end architecture;
