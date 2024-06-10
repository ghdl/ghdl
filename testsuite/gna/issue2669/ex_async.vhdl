library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ex_async is
  port(
    clk : in std_logic;
    req, ack : std_logic;
    reset : in std_logic
  );
end;

architecture arch of ex_async is
begin
  assert always (req -> eventually! ack) async_abort reset = '1' @rising_edge(clk);
end arch;

