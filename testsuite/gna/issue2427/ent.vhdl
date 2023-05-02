entity ent is
end;

use work.pkg.all;
architecture behav of ent is
begin
  assert my_type'bits = 48 severity failure;
  assert my_record'bits = 6 severity failure;
end behav;
