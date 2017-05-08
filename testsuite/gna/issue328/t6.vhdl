entity t6 is
   port (s : inout natural := 6);
end;

architecture behav of t6 is
begin
  process
  begin
    s <= s + 1;
    wait for 1 ns;
    assert s = 7 severity failure;
    wait;
  end process;
end behav;
