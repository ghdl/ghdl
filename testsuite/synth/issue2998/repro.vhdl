library ieee;
use ieee.std_logic_1164.all;

entity repro is
  generic (
    a : bit_vector(7 downto 0) := x"c1");
  port (b : out std_logic_vector(7 downto 0));
end;

architecture arch of repro is
begin
  b <= to_x01(a);
end arch;
