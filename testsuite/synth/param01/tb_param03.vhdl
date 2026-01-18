entity tb_param03 is
end tb_param03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_param03 is
  signal inp, res : std_logic_vector(3 downto 0);
begin
  dut: entity work.param03
    port map (add => inp, res => res);

  process
  begin
    inp <= x"0";
    wait for 1 ns;
    assert res = x"b" severity failure;

    inp <= x"3";
    wait for 1 ns;
    assert res = x"e" severity failure;

    wait;
  end process;
end behav;
