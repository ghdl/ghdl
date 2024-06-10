library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ex_sync is
  port(
    clk : in std_logic;
    req, ack : std_logic;
    reset : in std_logic
  );
end;

architecture arch of ex_sync is
begin
  assert always (req -> eventually! ack) sync_abort reset = '1' @rising_edge(clk);
end arch;

