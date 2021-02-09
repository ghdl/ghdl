library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity psl_onehot_err is
  port (clk  : in std_logic;
        a, b : in std_logic_vector(3 downto 0);
        c    : in natural range 0 to 15
  );
end entity psl_onehot_err;


architecture psl of psl_onehot_err is
begin

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion holds
  ONEHOT_0_a : assert always onehot(a);

  -- This assertion fails at cycle 12
  ONEHOT_1_a : assert always onehot(b);

  -- This assertion fails at cycle 12
  ONEHOT_2_a : assert always onehot(c);

end architecture psl;
