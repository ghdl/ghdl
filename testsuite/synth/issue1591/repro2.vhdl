library ieee;
  use ieee.std_logic_1164.all;

entity repro1 is
  port (clk : in std_logic);
end entity repro1;

architecture psl of repro1 is
begin
  testG : if true generate
    signal b : boolean := true;
  begin
    -- All is sensitive to rising edge of clk
    default clock is rising_edge(clk);

    -- This assertion generates an ghdl-yosys-plugin error
    -- ERROR: Assert `n.id != 0' failed in src/ghdl.cc:204.
    INITIAL_1_a : assert always b;
  end generate testG;
end architecture psl;
