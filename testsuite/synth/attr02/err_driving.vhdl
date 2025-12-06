entity err_driving is
  port (i : bit; o1, o : out bit);
end;

architecture behav of err_driving is
begin
  process (i)
  begin
    o <= '0';
    if o1'driving then
      o <= '1';
    end if;
    o1 <= not i;
  end process;
end;
