entity tb_func08 is
end tb_func08;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_func08 is
  signal v : std_ulogic_vector(31 downto 0);
  signal r : integer;
begin
  dut: entity work.func08
    port map (v, r);

  process
  begin
    v <= x"00000000";
    wait for 1 ns;
    assert r = 32 severity failure;

    v <= x"0000_0001";
    wait for 1 ns;
    assert r = 31 severity failure;

    v <= x"8000_0000";
    wait for 1 ns;
    assert r = 0 severity failure;

    v <= x"0001_00f0";
    wait for 1 ns;
    assert r = 15 severity failure;

    wait;
  end process;
end behav;
