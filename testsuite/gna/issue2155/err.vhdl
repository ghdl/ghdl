use work.closely_related_arrays.all;

entity e is
end;

architecture behav of e is
begin
  assert sv (1) = sv (1) severity failure;
end behav;
