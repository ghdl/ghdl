use work.pkg.all;

entity riassoc01 is
  port (v : nat_rec;
        res : out natural);
end riassoc01;

architecture behav of riassoc01 is
begin
  res <= v.a + v.b;
end behav;

entity iassoc01 is
  port (a, b : natural;
        res : out natural);
end iassoc01;

architecture behav of iassoc01 is
begin
  inst : entity work.riassoc01
    port map (v.a => a, v.b => b, res => res);
end behav;
