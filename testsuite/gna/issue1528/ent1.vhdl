library ieee;
use ieee.std_logic_1164.all;

use work.attrs_pkg.all;

entity ent1 is
  attribute ent_name of ent1: entity is "entity ent1";
  attribute ent_type of ent1: entity is "LogicI";
  attribute ent_stat of ent1: entity is 95;
end entity ent1;

architecture rtl of ent1 is
  attribute arc_name of rtl: architecture is "rtl of ent1";
  attribute arc_type of rtl: architecture is "RTL";
  attribute arc_stat of rtl: architecture is 100;
begin
  process
  begin
    report "I am: " & rtl'arc_name;
    report "Type: " & rtl'arc_type;
    report "Status: " & integer'image(rtl'arc_stat);
    wait;
  end process;
end rtl;
