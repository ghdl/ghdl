library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity conv03 is
end;

architecture arch of conv03 is
begin
  process
    variable v : signed (3 downto 0) := x"9";
  begin
    assert conv_std_logic_vector(v, 8) = std_logic_vector'(x"f9");
    wait;
  end process;
end;
