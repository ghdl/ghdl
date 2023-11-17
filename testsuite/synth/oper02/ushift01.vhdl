library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity Ushift01 is
  port(
    v    : in  unsigned(7 downto 0);
    n    : natural;
    rlo   : out unsigned(7 downto 0);
    llo   : out unsigned(7 downto 0);
    rao   : out unsigned(7 downto 0);
    lao   : out unsigned(7 downto 0)
  );
end Ushift01;

architecture rtl of Ushift01 is
begin
  rlo <= v srl n;
  llo <= v sll n;
  rao <= v sra n;
  lao <= v sla n;
end rtl;
