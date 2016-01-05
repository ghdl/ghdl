/* Using vhdl 2008 comments.  */
entity simple08 is
end;

architecture behav of simple08 is
begin
  process
  begin
    assert false report "Test is running" severity note;
    wait; -- Indefinite
  end process;
end behav;

