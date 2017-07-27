entity e is end entity;
architecture a of e is
  type ar1 is array(integer, integer) of integer;
  type ar2 is array(integer range -2e9 to 2e9, integer range -2e9 to 2e9) of integer;
  type ar3 is array(integer range -1e9 to 2e9, integer range -1e9 to 2e9) of integer;
begin
end architecture;
