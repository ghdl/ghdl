entity repro is
end repro;

entity comp is
  port (s : bit);
end comp;

architecture behav of repro is
  component comp is
    port (s : bit);
  end component;
  signal s : bit;
begin
  c : comp port map (s);
end behav;

use work.pkg.all;
entity comp2 is
  port (s : bit);
end comp2;

architecture behav of comp is
  component comp2 is
    port (s : bit);
  end component;
begin
  c2: comp2 port map (s);
end behav;
