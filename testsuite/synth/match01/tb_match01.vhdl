entity tb_match01 is
end tb_match01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_match01 is
  signal a : std_logic_vector(3 downto 0);
  signal z : std_logic;
begin
  dut: entity work.match01
    port map (a, z);

  process
  begin
    a <= "1000";
    wait for 1 ns;
    assert z = '1' severity failure;

    a <= "1010";
    wait for 1 ns;
    assert z = '1' severity failure;

    a <= "0000";
    wait for 1 ns;
    assert z = '0' severity failure;

    a <= "0001";
    wait for 1 ns;
    assert z = '0' severity failure;

    wait;
  end process;
end behav;
