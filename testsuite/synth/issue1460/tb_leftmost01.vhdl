entity tb_leftmost01 is
end tb_leftmost01;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_leftmost01 is
  signal a : unsigned (7 downto 0);
  signal b : unsigned (8 to 12);
  signal c : unsigned (0 to 3);
  signal ra, rb, rc : integer;
begin
  dut_a: entity work.leftmost01
    port map (a, ra);
--  dut_b: entity work.leftmost01
--    port map (b, rb);
--  dut_c: entity work.leftmost01
--    port map (c, rc);

  process
  begin
    a <= b"0010_0101";
    b <= b"00101";
    c <= b"0000";

    wait for 1 ns;

    assert ra = 5 severity failure;
--    assert rb = 10 severity failure;
--    assert rc = -1 severity failure;

    wait;
  end process;
end behav;
