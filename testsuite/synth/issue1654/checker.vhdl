library ieee;
  use ieee.std_logic_1164.all;

entity checker is
  port (
    clk : in std_logic;
    a, b, c, d : std_logic
  );
end;


architecture psl of checker is
begin
  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion holds
  WITH_sync_ABORT_a : assert (always a -> next (b before a)) sync_abort c;

  -- This assertion should also hold, but it does not
  -- GHDL seems to implement abort as sync_abort instead of async_abort
  -- See 1850-2010 6.2.1.5.1 abort, async_abort, and sync_abort
  WITH_async_ABORT_a : assert (always a -> next (b before a)) async_abort d;
end architecture psl;

