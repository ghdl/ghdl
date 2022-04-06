library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
  port (p : out std_ulogic_vector(3 downto 0));
  signal s : std_ulogic_vector(3 downto 0);
end entity;

architecture a of repro2 is
begin
  s <= x"3";
  p <= s;
end architecture;
