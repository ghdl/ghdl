library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity Ushift02 is
  port(
    v    : in  unsigned(7 downto 0);
    llo   : out unsigned(7 downto 0)
  );
end Ushift02;

architecture rtl of Ushift02 is
begin
  process (v)
    variable n : integer;
  begin
    n := 5;
    llo <= v sll n;
  end process;
end rtl;
