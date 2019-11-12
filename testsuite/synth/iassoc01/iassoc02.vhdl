use work.pkg.all;

entity riassoc02 is
  port (v : natural;
        res : out nat_rec);
end riassoc02;

architecture behav of riassoc02 is
begin
  res.a <= v + 1;
  res.b <= v + 2;
end behav;

entity iassoc02 is
  port (v : natural;
        a, b : out natural);
end iassoc02;

architecture behav of iassoc02 is
begin
  inst : entity work.riassoc02
    port map (v => v, res.a => a, res.b => b);
end behav;
