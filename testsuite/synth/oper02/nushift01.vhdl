library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std_unsigned.all;

entity Nushift01 is
  port(
    v    : in  std_logic_vector(7 downto 0);
    n    : natural;
    rlo   : out std_logic_vector(7 downto 0);
    llo   : out std_logic_vector(7 downto 0);
    rao   : out std_logic_vector(7 downto 0);
    lao   : out std_logic_vector(7 downto 0)
  );
end Nushift01;

architecture rtl of Nushift01 is
begin
  rlo <= v srl n;
  llo <= v sll n;
  rao <= v sra n;
  lao <= v sla n;
end rtl;
