entity tb1 is
  package pkg1 is
    constant c : natural := 5;
    function f return natural;
  end pkg1;
end tb1;

architecture behav of tb1 is
begin
  assert pkg1.c = 5 severity failure;
  assert pkg1.c /= 5 report "value is correct" severity note;
end behav;
