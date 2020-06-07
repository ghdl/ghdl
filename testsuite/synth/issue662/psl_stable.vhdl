library ieee;
use ieee.std_logic_1164.all;

entity psl_stable is
  port (clk, a, b, c : in std_logic;
        d         : in std_logic_vector(3 downto 0)
  );
end entity psl_stable;


architecture psl of psl_stable is
begin

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion should fail at cycle 13
  STABLE_0_a : assert always {not a; a} |=> (stable(c) until_ b);

  -- This assertion holds
  STABLE_1_a : assert always {not a; a} |=> (stable(d) until_ b);

  -- This assertion should fail at cycle 13
  STABLE_2_a : assert always {not a; a} |=> (c = prev(c) until_ b);

  -- This assertion holds
  STABLE_3_a : assert always {not a; a} |=> (d = prev(d) until_ b);

end architecture psl;
