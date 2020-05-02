entity assert2 is
  port (v : natural;
        en : boolean;
        res : out natural);
end assert2;

architecture behav of assert2 is
begin
  process (v, en)
  begin
    if en then
      assert v < 10 report "bad v value";
      res <= v + 1;
    else
      res <= 0;
    end if;
  end process;
end behav;

