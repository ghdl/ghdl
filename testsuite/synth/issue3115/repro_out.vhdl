entity sub is
  port (a : bit;
        o : out bit);
  attribute syn_keep                 : boolean;
  attribute syn_keep of o         : signal is true;
end sub;

architecture behav of sub is
  signal t : bit;
begin
  t <= not a;
  o <= t;
end behav;

entity repro is
  port (a : bit;
        b : out bit);
end repro;

architecture behav of repro is
begin
  dut : entity work.sub port map (a, b);
end behav;
