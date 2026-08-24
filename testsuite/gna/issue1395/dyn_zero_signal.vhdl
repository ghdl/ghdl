-- Same underlying issue as dyn_zero.vhdl (a POSITIVE-indexed STRING with an
-- index range that includes 0), but a different declaration shape: a
-- *signal* with a default aggregate value, declared at the architecture
-- level (not a variable inside a process), and with the generic on the
-- *left* bound instead of the right. See issue1395.
entity dyn_zero_signal is
  generic (g : integer := 4);
end entity;

architecture arch of dyn_zero_signal is
  signal str : string (g downto 0) := (others => 'a');
begin
  process
  begin
    report str;
    wait;
  end process;
end architecture;
