architecture arch of e is
  signal s, s_n : bit;
begin
  --  Comment
  p : process (s)
  begin
    s <= not s_n;
  end process;
end arch;
