library ieee;
use ieee.std_logic_1164.all;

entity test2 is
  port (led: out std_logic_vector (7 downto 0));
end test2;

architecture synth of test2 is

begin
  led(7) <= '0';
  led(6) <= '1';
--  led(5) <= '0';
--  led(3 downto 0) <= x"9";
end synth;
