entity tb_case02 is
end tb_case02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_case02 is
  signal a : std_logic_vector (1 downto 0);
  signal o : std_logic;
begin
  dut: entity work.case02
    port map (a, o);

  process
  begin
    a <= "00";
    wait for 1 ns;
    assert o = '0';

    a <= "01";
    wait for 1 ns;
    assert o = '1';

    a <= "10";
    wait for 1 ns;
    assert o = '0';

    wait;
  end process;
end behav;
