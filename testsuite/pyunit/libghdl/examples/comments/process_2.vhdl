architecture arch of e is
  signal s, s_n : bit;
begin
  p : process (s)
    --  Comment for :p:

    -- For :v:
    variable v : boolean;
  begin
    s <= not s_n;
  end process;
end arch;
