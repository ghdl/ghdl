library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

entity ifgen1 is
end;

architecture behav of ifgen1 is
  signal top : natural := 123;
begin
  g_true: if true generate
    signal s : bit := '1';
  begin
    s <= '0' after 4 ns;
  end generate;

  g_false: if false generate
    signal s : std_logic;
  begin
    s <= '1' after 3 ns;
  end generate;
end behav;
