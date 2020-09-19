entity tb_rightmost02 is
end tb_rightmost02;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_rightmost02 is
  signal b : signed (2 to 4);
  signal rb : integer;
begin
  dut_b: entity work.rightmost02
    port map (b, rb);

  process
  begin
    b <= b"010";

    wait for 1 ns;

    assert rb = 3 severity failure;

    wait;
  end process;
end behav;
