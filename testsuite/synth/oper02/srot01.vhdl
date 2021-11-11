library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity Srot01 is
  port(
    v    : in  signed(7 downto 0);
    ro   : out signed(7 downto 0);
    lo   : out signed(7 downto 0)
  );
end Srot01;

architecture rtl of Srot01 is
begin
  ro <= v ror 1;
  lo <= v rol 1;
end rtl;
