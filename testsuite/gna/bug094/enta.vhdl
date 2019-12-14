use work.pkga.all;

entity enta is
  port (clk : bit;
        data : word);
end enta;

architecture behav of enta is
begin
  process (clk)
    variable prev : word;
  begin
    if clk = '1' then
      if prev /= data then
        report "data has changed" severity note;
        prev := data;
      end if;
    end if;
  end process;
end behav;
