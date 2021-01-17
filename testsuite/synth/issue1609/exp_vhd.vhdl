library IEEE;
  use IEEE.std_logic_1164.ALL;
  use IEEE.numeric_std.ALL;

entity exp is
  port (
    clk : in    std_logic
  );
end entity exp;

architecture behav of exp is

  signal any_nat : natural;

  attribute anyconst : boolean;
  attribute anyconst of any_nat : signal is true;

begin

  default Clock is rising_edge(clk);

  assume always any_nat >=10;
  assert always any_nat >=10;

end architecture behav;

