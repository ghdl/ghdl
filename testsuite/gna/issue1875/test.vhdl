library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.VITAL_timing.all;
use IEEE.VITAL_primitives.all;

entity test is
  port (
    CLK : in std_logic_vector(1 downto 0)
    );
  attribute VITAL_LEVEL0 of test : entity is TRUE;
end test;

architecture VITAL_ACT of test is
begin
  process
  begin
  wait;
  end process;
end architecture;
