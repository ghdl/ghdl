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

  -- Error occurs when using a generate statement
  testG : if true generate

    signal b : natural;
    attribute anyconst of b : signal is true;

  begin

    -- works
    GEN_ASSUME : assume always b = 23;
    GEN_ASSERT : assert always b = 23;

  end generate testG;

  -- Same error occurs when using a block statement
  testB : block is

    signal c : natural;
    attribute anyconst of c : signal is true;

  begin

    -- works
    BLK_ASSUME : assume always c = 11;
    BLK_ASSERT : assert always c = 11;

  end block testB;


end architecture psl;
