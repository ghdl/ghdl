library ieee;
  use ieee.std_logic_1164.all;


entity issue is
  port (
    clk : in std_logic
  );
end entity issue;


architecture psl of issue is

  signal a : boolean := true;

begin


  testG : if true generate

    signal b : boolean := true;
    signal c : boolean := false;

  begin

    c <= true;

    -- All is sensitive to rising edge of clk
    default clock is rising_edge(clk);

    -- This assertion works
    INITIAL_0_a : assert always a;

    -- This assertion generates an ghdl-yosys-plugin error
    -- ERROR: Assert `n.id != 0' failed in src/ghdl.cc:204.
    INITIAL_1_a : assert always b;

    -- This assertion works
    INITIAL_2_a : assert always c;

  end generate testG;

  -- Same error occurs when using a block instead of a generate
  -- statement
  testB : block is

    signal b : boolean := true;
    signal c : boolean := false;

  begin

    c <= true;

    -- All is sensitive to rising edge of clk
    default clock is rising_edge(clk);

    -- This assertion works
    INITIAL_0_a : assert always a;

    -- This assertion generates an ghdl-yosys-plugin error
    -- ERROR: Assert `n.id != 0' failed in src/ghdl.cc:204.
    INITIAL_1_a : assert always b;

    -- This assertion works
    INITIAL_2_a : assert always c;

  end block testB;


end architecture psl;
