library ieee;
use ieee.std_logic_1164.all;

entity psl_fell1 is
  port (clk, a, b : in std_logic
  );
end entity psl_fell1;


architecture psl of psl_fell1 is
begin

  -- This assertion holds
  FELL_0_a : assert always {a; not a} |-> fell(a);

end architecture psl;
