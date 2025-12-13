entity tb_loop05 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_loop05 is
  signal v : std_logic_vector(3 downto 0);
  signal r : integer;
begin
  dut: entity work.loop05
    port map (v, r);

  process
  begin
    v <= b"1000";
    wait for 1 ns;
    assert r = 3 severity failure;

    v <= b"1100";
    wait for 1 ns;
    assert r = 3 severity failure;

    v <= b"1001";
    wait for 1 ns;
    assert r = 0 severity failure;

    v <= b"1110";
    wait for 1 ns;
    assert r = 1 severity failure;

    v <= b"0000";
    wait for 1 ns;
    assert r = -1 severity failure;

    wait;
  end process;
end behav;
