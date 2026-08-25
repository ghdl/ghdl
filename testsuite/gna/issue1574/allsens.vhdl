entity allsens is end;
architecture a of allsens is
  signal s : bit := '0';
  signal o : bit := '0';
  --  Declared pure but reads a signal, so the process below has to be
  --  sensitive to it even though it only calls the function.
  pure function f return bit is
  begin
    return s;
  end f;
begin
  process (all)
  begin
    o <= f;
  end process;
  process
  begin
    wait for 10 ns;
    s <= '1';
    wait for 10 ns;
    report "o = '" & bit'image(o) & "'";
    wait;
  end process;
end a;
