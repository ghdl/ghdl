entity tb_leftmost03 is
end tb_leftmost03;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_leftmost03 is
  signal c : unsigned (0 to 8);
  signal rc : integer;
begin
  dut_c: entity work.leftmost03
    port map (c, rc);

  process
  begin
    c <= b"0_0000_0000";

    wait for 1 ns;

    assert rc = -1 severity failure;

    wait;
  end process;
end behav;
