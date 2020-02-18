entity tb_iassoc04 is
end tb_iassoc04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_iassoc04 is
  signal a, b : bit_vector (3 downto 0);
  signal res : bit;
begin
  dut: entity work.iassoc04
    port map (a, b, res);

  process
  begin
    a <= "0001";
    b <= "0000";
    wait for 1 ns;
    assert res = '1' severity failure;

    a <= "0000";
    b <= "0000";
    wait for 1 ns;
    assert res = '0' severity failure;

    wait;
  end process;
end behav;
