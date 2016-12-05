entity tb2 is
end tb2;

architecture behav of tb2 is
  package pkg1 is
    generic (c : natural);
    generic map (c => 5);

    function f return natural;
  end pkg1;

  package body pkg1 is
    function f return natural is
    begin
      return c;
    end f;
  end pkg1;
begin
  assert pkg1.f = 5 severity failure;
end behav;
