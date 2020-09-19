entity tb_leftmost02 is
end tb_leftmost02;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_leftmost02 is
  signal b : signed (8 to 12);
  signal rb : integer;
begin
  dut_b: entity work.leftmost02
    port map (b, rb);

  process
  begin
    b <= b"00101";

    wait for 1 ns;

    assert rb = 10 severity failure;

    wait;
  end process;
end behav;
