library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity Urot01 is
  port(
    v    : in unsigned(7 downto 0);
    ro   : out unsigned(7 downto 0);
    lo   : out unsigned(7 downto 0)
  );
end Urot01;

architecture rtl of Urot01 is
begin
  ro <= v ror 1;
  lo <= v rol 1;
end rtl;
