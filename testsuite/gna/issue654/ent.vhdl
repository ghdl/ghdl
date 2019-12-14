entity ent is
end ent;

architecture behav of ent is
  signal s : bit;
begin
  s <= not s;
end behav;
