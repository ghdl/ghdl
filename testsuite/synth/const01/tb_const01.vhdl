entity tb_const01 is
end tb_const01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_const01 is
  signal res : std_logic_vector(31 downto 0);
begin
  dut: entity work.const01
    port map (res);

  process
  begin
    wait for 1 ns;
    assert res = x"01020304" severity failure;

    wait;
  end process;
end behav;
