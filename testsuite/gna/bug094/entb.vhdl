use work.pkgb.all;

entity entb is
  port (clk : bit;
        val : natural);
end entb;

architecture behav of entb is
begin
  process (clk)
  begin
    if clk = '1' then
      v := val;
    end if;
  end process;
end behav;
