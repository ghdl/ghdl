library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity vector8_test1 is
  port (led7: out std_logic);
end vector8_test1;

architecture synth of vector8_test1 is

signal v : std_logic_vector(7 downto 0);

begin
  v(7) <= '1';
  led7 <= v(7);
end synth;
