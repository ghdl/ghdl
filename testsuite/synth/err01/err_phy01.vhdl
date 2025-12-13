entity err_phy01 is
  port (a : in bit;
        o : out boolean);
end;

architecture behav of err_phy01 is
begin
  process (a)
    variable t : time;
  begin
    if a = '1' then
      t := 10 ns;
    else
      t := 20 ns;
    end if;
    o <= (t / 1 ns) > 0;
  end process;
end behav;
