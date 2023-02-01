package pkg3 is
  generic (c : natural);

  constant d : natural := c;
end;

entity tb3 is
end tb3;

architecture behav of tb3 is
  package p is new work.pkg3 generic map (c => 3);
begin
  assert p.d = 3;
end behav;
