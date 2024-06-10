library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo is
  port(
    clk : in std_logic;
    reset : in std_logic
  );
end foo;

architecture arch of foo is
  signal a : std_logic;
begin
  assert always (a = '1')       abort (reset = '1') @rising_edge(clk); -- ok
  assert always (a = '1')  sync_abort (reset = '1') @rising_edge(clk); -- ok
  assert always (a = '1') async_abort (reset = '1') @rising_edge(clk); -- error
--  assert (always a = '1') async_abort (reset = '1') @rising_edge(clk); -- error
end arch;

