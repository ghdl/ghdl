entity tb2 is
end;

architecture behav of tb2 is
  signal s : bit;
begin
  process
  begin
    for i in 1 to 4 loop
      s <= not s;
      wait for 5 ns;
    end loop;
    --  20 ns
    wait for 4 ns;
    wait;
  end process;
end behav;
