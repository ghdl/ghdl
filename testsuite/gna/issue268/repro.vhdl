entity repro is
end repro;

architecture behav of repro is
  signal s : bit;
begin
  s <= '0' after 2 ns, '1' after 4 ns;
end behav;
