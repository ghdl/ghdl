use work.pkg.all;

entity riassoc11 is
  port (v : nat_rec;
        res : out natural);
end riassoc11;

architecture behav of riassoc11 is
begin
  res <= v.a + v.b;
end behav;

entity iassoc11 is
  port (a, b : natural;
        res : out natural);
end iassoc11;

use work.pkg.all;

architecture behav of iassoc11 is
  component riassoc11 is
    port (v : nat_rec;
          res : out natural);
  end component;
begin
  inst : riassoc11
    port map (v.a => a, v.b => b, res => res);
end behav;
