entity convflt02 is
  generic (s : real := -1.0);
end;

architecture behav of convflt02 is
begin
  process
    subtype my_rng is integer range 10 downto 0;
    variable v : my_rng;
  begin
    v := my_rng(s);
    wait;
  end process;
end;
