entity proc2 is
  port (clk : bit; o : out bit);
end proc2;

architecture behav of proc2 is
begin
  process (clk) is
    variable v : bit;
  begin
    if clk'event and clk = '1' then
      v := not v;
    end if;
    o <= v;
  end process;
end behav;
