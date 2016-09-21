package pkg1 is
  generic (c : natural);
  --  ??? Looks invalid, but what is the rule ?
  generic map (c => c);
end pkg1;

entity tb1 is
end;

architecture behav of tb1 is
begin
  assert true;
end behav;
