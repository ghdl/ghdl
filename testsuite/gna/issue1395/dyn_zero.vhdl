-- STRING is `array (POSITIVE range <>) of CHARACTER`, so any index subtype
-- constraint including 0 is invalid (POSITIVE starts at 1). Here the bound
-- is not locally static (it comes from a generic), so the violation can
-- only be caught by a runtime/elaboration-time check, not by a purely
-- static analysis-time check. See issue1395.
entity dyn_zero is
  generic (n : integer := 0);
end entity;

architecture a of dyn_zero is
begin
  process
    variable s : string(4 downto n);
  begin
    report "len=" & integer'image(s'length);
    wait;
  end process;
end architecture;
