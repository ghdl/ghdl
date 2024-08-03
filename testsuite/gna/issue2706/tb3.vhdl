entity tb3 is
end;

architecture behav of tb3 is
  signal s : bit;
begin
  process
  begin
    for i in 1 to 5 loop
      s <= not s;
      wait for 5 ns;
    end loop;

    wait for 5 ns;
    s <= '0';
    wait for 0 ns;
    assert s = '1' severity failure;
    wait;
  end process;
end behav;
