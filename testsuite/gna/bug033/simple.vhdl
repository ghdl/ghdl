entity simple is
end;

architecture behav of simple is
begin
  process
  begin
    assert false report "Test is running" severity note;
    wait; -- Indefinite
  end process;
end behav;

