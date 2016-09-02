entity tb_simple1 is
  generic (v : natural := 4);
  package pkg1 is
    constant c : natural := v + 1;
  end pkg1;
end tb_simple1;

architecture behav of tb_simple1 is
begin
  assert pkg1.c = 5 severity failure;
  assert pkg1.c /= 5 report "value is correct" severity note;
end behav;
