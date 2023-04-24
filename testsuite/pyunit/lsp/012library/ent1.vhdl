entity ent1 is
end;

library lib1;
use lib1.pkg1.all;
architecture behav of ent1 is
begin
  assert c = 12 severity failure;
end;
