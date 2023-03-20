use work.frequency.all;

entity tb_freq is
  port (clk : inout bit);
end;

architecture behav of tb_freq is
begin
  generate_clock_bit.generate_clock (clk, 10 Mhz, 12);
end behav;
