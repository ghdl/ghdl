library IEEE;
use IEEE.std_logic_1164.ALL;

entity err_attr02 is
  port (clk : std_logic);
end;

architecture behav of err_attr02 is
  constant any_nat : natural := 5;

  attribute anyconst : boolean;
  attribute anyconst of any_nat : constant is true;
begin
  default Clock is rising_edge(clk);

  assert always any_nat >=10;
end architecture behav;

