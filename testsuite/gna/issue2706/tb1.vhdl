entity tb1 is
end;

architecture behav of tb1 is
  signal s : bit;
begin
  process
  begin
    for i in 1 to 4 loop
      s <= not s;
      wait for 5 ns;
    end loop;
    wait;
  end process;
end behav;
