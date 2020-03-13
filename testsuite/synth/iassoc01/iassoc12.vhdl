use work.pkg.all;

entity riassoc12 is
  port (v : natural;
        res : out nat_rec);
end riassoc12;

architecture behav of riassoc12 is
begin
  res.a <= v + 1;
  res.b <= v + 2;
end behav;

entity iassoc12 is
  port (v : natural;
        a, b : out natural);
end iassoc12;

use work.pkg.all;

architecture behav of iassoc12 is
  component riassoc12 is
    port (v : natural;
          res : out nat_rec);
  end component;
begin
  inst : riassoc12
    port map (v => v, res.a => a, res.b => b);
end behav;
