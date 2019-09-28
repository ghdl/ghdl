entity ent is
end ent;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture rtl of ent is
  signal test : std_logic;
begin
  -- ent.vhdl:5:12
  test <= <unassigned>; -- (signal)
end rtl;
