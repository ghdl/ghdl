entity tb_func08b is
end tb_func08b;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_func08b is
  signal v : std_ulogic_vector(3 downto 0);
  signal r : integer;
begin
  dut: entity work.func08b
    port map (v, r);

  process
  begin
    v <= x"0";
    wait for 1 ns;
    assert r = 4 severity failure;

    v <= x"1";
    wait for 1 ns;
    assert r = 3 severity failure;

    v <= x"8";
    wait for 1 ns;
    assert r = 0 severity failure;

    v <= x"3";
    wait for 1 ns;
    assert r = 2 severity failure;

    wait;
  end process;
end behav;
