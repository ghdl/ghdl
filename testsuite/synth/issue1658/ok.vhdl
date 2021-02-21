library ieee;
  use ieee.std_logic_1164.all;


entity issue is
  port (
    clk : in std_logic
  );
end entity issue;

architecture psl of issue is

  attribute anyconst : boolean;
  signal a: natural;
  attribute anyconst of a : signal is true;

begin

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- works
  assume always a = 42;
  assert always a = 42;

end architecture;
