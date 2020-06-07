library ieee;
use ieee.std_logic_1164.all;

entity psl_fell is
  port (clk, a, b : in std_logic
  );
end entity psl_fell;


architecture psl of psl_fell is
begin

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion holds
  FELL_0_a : assert always {a; not a} |-> fell(a);

  -- This assertion holds
  FELL_1_a : assert always (fell(a) -> (prev(a) and not a));

  -- This assertion should fail at cycle 11
  FELL_2_a : assert always fell(a) -> b;

end architecture psl;
