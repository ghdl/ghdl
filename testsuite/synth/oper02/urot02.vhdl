library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity Urot02 is
  port(
    v    : in unsigned(2 downto 0);
    q    : in unsigned(7 downto 0);
    lo   : out unsigned(2 downto 0)
  );
end Urot02;

architecture rtl of Urot02 is
begin
  lo <= v rol to_integer(q);
end rtl;
