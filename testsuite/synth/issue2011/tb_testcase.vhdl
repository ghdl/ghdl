entity tb_testcase is
end tb_testcase;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_testcase is
  signal sel : std_ulogic_vector(3 downto 0);
  signal result   : std_ulogic_vector(63 downto 0);
begin
  dut: entity work.testcase
    port map (sel, result);

  process
  begin
    sel <= "0000";
    wait for 1 ns;
    assert result = x"00000000_00000000";

    sel <= "1101";
    wait for 1 ns;
    assert result = x"ffffffff_00000000";
    wait;
  end process;
end behav;
