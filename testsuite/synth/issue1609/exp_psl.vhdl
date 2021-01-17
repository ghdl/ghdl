library IEEE;
  use IEEE.std_logic_1164.ALL;
  use IEEE.numeric_std.ALL;

entity exp is
  port ( clk : in std_logic );
end entity exp;

architecture behav of exp is
begin
end architecture behav;
vunit exp_formal (exp(behav))
{
    signal any_nat : natural;

    attribute anyconst : boolean;
    attribute anyconst of any_nat : signal is true;

    default Clock is rising_edge(clk);

    assume always any_nat >=10;
    assert always any_nat >=10;
}
