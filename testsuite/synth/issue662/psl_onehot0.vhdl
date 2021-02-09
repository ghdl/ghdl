library ieee;
use ieee.std_logic_1164.all;

entity psl_onehot0 is
  port (clk  : in std_logic;
        a, b : in std_logic_vector(3 downto 0)
  );
end entity psl_onehot0;


architecture psl of psl_onehot0 is
begin

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion holds
  ONEHOT0_0_a : assert always onehot0(a);

  -- This assertion fails at cycle 15
  ONEHOT0_1_a : assert always onehot0(b);

end architecture psl;
