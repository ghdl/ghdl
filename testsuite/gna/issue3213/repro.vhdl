

library ieee;
use ieee.std_logic_1164.all;

entity repro_assert is

  port (
    the_input  : in  std_logic_vector;
    the_output : out std_logic_vector(1 downto 0));

end entity repro_assert;

architecture arch of repro_assert is
begin
  -- The telephone rang and, after, I forgot to complete "> 1"
  assert the_input'length report "The input length should be at least 2" severity failure;
end architecture arch;

