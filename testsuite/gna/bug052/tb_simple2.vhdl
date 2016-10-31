entity tb_simple2 is
  generic (v : natural := 4);
  package pkg1 is
    constant c : natural := v + 1;
  end pkg1;
  constant c : natural := v - 1;
end;

architecture behav of tb_simple2 is
begin
  assert pkg1.c = 5 and c = 3 severity failure;
  assert not (pkg1.c = 5 and c = 3) report "value is correct" severity note;
end behav;
