library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity conv02 is
end;

architecture arch of conv02 is
begin
  process
    variable v : integer := 5;
  begin
    assert conv_integer(v) = v;
    wait;
  end process;
end;
