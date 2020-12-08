use work.attrs_pkg.all;

entity ent3 is
  attribute ent_name of ent3: entity is "entity ent3";
  attribute ent_type of ent3: entity is "LogicIII";
  attribute ent_stat of ent3: entity is 45;
end entity ent3;

architecture rtl of ent3 is
  attribute arc_name of rtl: architecture is "rtl of ent3";
  attribute arc_type of rtl: architecture is "RTL";
  attribute arc_stat of rtl: architecture is 35;
begin
  process
  begin
    report "I am: " & rtl'arc_name;
    report "Type: " & rtl'arc_type;
    report "Status: " & integer'image(rtl'arc_stat);
    wait;
  end process;
end rtl;
