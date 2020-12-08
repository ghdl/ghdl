use work.attrs_pkg.all;

entity ent2 is
  attribute ent_name of ent2: entity is "entity ent2";
attribute ent_type of ent2: entity is "LogicII";
attribute ent_stat of ent2: entity is 75;
end entity ent2;

architecture bhv of ent2 is
attribute arc_name of bhv: architecture is "bhv of ent2";
attribute arc_type of bhv: architecture is "BHV";
attribute arc_stat of bhv: architecture is 100;
begin
process
begin
report "I am: " & bhv'arc_name;
report "Type: " & bhv'arc_type;
report "Status: " & integer'image(bhv'arc_stat);
wait;
end process;
end bhv;
