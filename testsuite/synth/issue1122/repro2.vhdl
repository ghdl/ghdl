entity repro2 is
  port (clk : bit;
        rst : bit;
        d : bit;
        q : out bit);
end repro2;

architecture behav of repro2 is
  constant c : bit := '1';
  signal s : bit := c;
begin
  process (clk)
  begin
    if rst = '1' then
      s <= c;
    elsif clk = '1' and clk'event then
      s <= d;
    end if;
  end process;

  q <= s;
end behav;
