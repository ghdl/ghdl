package repro1_pkg1 is
  constant c : bit := '0';
end;

use work.repro1_pkg1.all;

entity comp1 is
  port (a : bit);
end;

architecture behav of comp1 is
begin
  assert a = '1' report "a is set" severity note;
end;
