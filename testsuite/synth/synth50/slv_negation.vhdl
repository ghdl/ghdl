library ieee;
  use ieee.std_logic_1164.all;

entity slv_negation is
  port (
    a : in std_logic_vector(7 downto 0);
    b : out std_logic_vector(7 downto 0)
  );
end slv_negation;

architecture rtl of slv_negation is
begin
  b <= not a(7 downto 0);
end rtl;
