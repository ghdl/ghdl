entity tb_test2 is
end tb_test2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test2 is
  signal addr : std_logic_vector(1 downto 0);
  signal data : std_logic_vector(63 downto 0);
begin
  dut: entity work.test2
    port map (addr, data);

  process
  begin
    addr <= b"00";
    wait for 1 ns;
    assert data = x"0102030405060708" severity failure;

    addr <= b"01";
    wait for 1 ns;
    assert data = x"1112131415161718" severity failure;

    addr <= b"10";
    wait for 1 ns;
    assert data = x"2122232425262728" severity failure;

    addr <= b"11";
    wait for 1 ns;
    assert data = x"0000000000000000" severity failure;

    wait;
  end process;
end behav;
