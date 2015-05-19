library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
end entity;

architecture a of ent is
begin
  main : process
  procedure proc(constant value : unsigned)  is
    variable a : integer := value'length;
    variable b : integer := (value'length - 1)/2;
  begin
    report "x       = " & integer'image(a);
    report "(x-1)/2 = " & integer'image(b);
    assert a = 0 and b = 0 severity failure;
  end procedure;

   variable value : unsigned(0 downto 1);
  begin
    proc(unsigned'(""));
    proc(value);
    wait;
  end process;
end architecture;
