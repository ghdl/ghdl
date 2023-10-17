package repro1_pkg2 is
  signal s2 : bit;
end;

use work.repro1_pkg2.all;

entity comp2 is
  port (a : bit);
end;

architecture behav of comp2 is
begin
  s2 <= a;
  
  assert s2 = '0' report "s2 is not set" severity note;
end;
