package myPackageX3 is
  constant c : string := ('a', 'b');
end;

entity repro3 is
end;

architecture behav of repro3 is
  alias myPackage is work.myPackageX3;

  constant e : string := myPackage.c;
begin
  assert e = "ab" severity failure;
end;
