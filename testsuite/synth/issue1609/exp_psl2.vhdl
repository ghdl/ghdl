library IEEE;
  use IEEE.std_logic_1164.ALL;
  use IEEE.numeric_std.ALL;

entity exp2 is
  port ( clk : in std_logic );
end;

architecture behav of exp2 is
begin
end architecture behav;

vunit exp_formal (exp2(behav))
{
    signal any_nat : natural;

    attribute allconst : boolean;
    attribute allconst of any_nat : signal is true;

    default Clock is rising_edge(clk);

    assume always any_nat >=10;
    assert always any_nat >=10;
}
