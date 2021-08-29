library ieee;
  use ieee.std_logic_1164.all;


entity issue is
  port (
    clk : in std_logic;
    a, b : in std_logic
  );
end entity issue;


architecture psl of issue is
begin

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion should hold
  INF_a : assert always {a} |=> {not b[*0 to inf]; b};
end architecture psl;
