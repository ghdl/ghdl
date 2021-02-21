library ieee;
  use ieee.std_logic_1164.all;


entity issue is
  port (
    clk : in std_logic
  );
end entity issue;

architecture psl of issue is

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

begin
  -- Error occurs when using a generate statement
  testG : if true generate

    attribute anyconst : boolean;
    signal b : natural;
    attribute anyconst of b : signal is true;

  begin

    -- don't work
    GEN_ASSUME : assume always b = 23;
    GEN_ASSERT : assert always b = 23;

  end generate testG;

end architecture;
