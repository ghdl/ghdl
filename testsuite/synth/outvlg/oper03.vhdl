library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity oper03 is
  port (clk : std_logic;
        v1 : unsigned (2 downto 0);
        v2 : unsigned (2 downto 0);
        o1, o2, o3, o4 : out unsigned (2 downto 0));
end oper03;

architecture behav of oper03 is
begin
  o1 <= minimum (v1, v2);
  o2 <= maximum (v1, v2);
  o3 <= v1 mod v2;
  o4 <= v1 rem v2;
end behav;
