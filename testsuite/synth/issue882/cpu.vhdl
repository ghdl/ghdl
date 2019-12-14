library ieee;
use ieee.std_logic_1164.all;

entity cpu is
  port (
    test : out std_logic_vector(7 downto 0)
  );
end cpu;

architecture rtl of cpu is
begin
  test <= "00000000";
end rtl;
