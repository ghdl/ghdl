library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity Sshift01 is
  port(
    v    : in  signed(7 downto 0);
    n    : natural;
    rlo   : out signed(7 downto 0);
    llo   : out signed(7 downto 0);
    rao   : out signed(7 downto 0);
    lao   : out signed(7 downto 0)
  );
end Sshift01;

architecture rtl of Sshift01 is
begin
  rlo <= v srl n;
  llo <= v sll n;
  rao <= v sra n;
  lao <= v sla n;
end rtl;
