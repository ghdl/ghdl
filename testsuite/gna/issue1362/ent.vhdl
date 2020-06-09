entity ent is
end;

library liba;
use liba.pkga.all;

library libb;
use libb.pkgb.all;

architecture behav of ent is
begin
  assert a + b = 7;
end;
