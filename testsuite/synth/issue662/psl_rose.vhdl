library ieee;
use ieee.std_logic_1164.all;

entity psl_rose is
  port (clk, a, b : in std_logic
  );
end entity psl_rose;


architecture psl of psl_rose is
begin

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion holds
  ROSE_0_a : assert always {not a; a} |-> rose(a);

  -- This assertion holds
  ROSE_1_a : assert always (rose(a) -> (not prev(a) and a));

  -- This assertion should fail at cycle 11
  ROSE_2_a : assert always rose(a) -> b;

end architecture psl;
