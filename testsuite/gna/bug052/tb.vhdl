entity tb is
  package pkg1 is
    constant c : natural := 5;
  end pkg1;
end tb;

architecture behav of tb is
begin
  assert pkg1.c = 5 severity failure;
  assert pkg1.c /= 5 report "value is correct" severity note;
end behav;
