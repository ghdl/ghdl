library ieee;
  use ieee.std_logic_1164.all;

entity repro3 is
  port (clk : in std_logic;
        o : out boolean);
end entity repro3;

architecture psl of repro3 is
begin
  testG : if true generate
    signal b : boolean := true;
  begin
    assert b;
  end generate testG;
end architecture psl;
