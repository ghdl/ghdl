entity proc1 is
  port (clk : bit; o : out bit);
end proc1;

architecture behav of proc1 is
begin
  process (clk)
  is
    variable v : bit;
  begin
    if clk'event and clk = '1'
    then
      v := not v;
    end if;
    o <= v;
  end process;
end behav;
