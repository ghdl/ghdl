entity tb_loop07 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_loop07 is
  signal v : std_logic_vector(7 downto 0);
  signal r : integer;
begin
  dut: entity work.loop07
    port map (v, r);

  process
  begin
    v <= b"0000_0000";
    wait for 1 ns;
    assert r = -1 severity failure;

    v <= b"0000_0001";
    wait for 1 ns;
    assert r = 0 severity failure;

    v <= b"0001_1000";
    wait for 1 ns;
    assert r = 4 severity failure;

    wait;
  end process;
end behav;
