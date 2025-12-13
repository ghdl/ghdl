entity tb_match08 is
end tb_match08;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_match08 is
  signal a : std_logic_vector(3 downto 0);
  signal z : std_logic;
begin
  dut: entity work.match08
    port map (a, z);

  process
  begin
    a <= "1000";
    wait for 1 ns;
    assert z = 'X' severity failure;

    a <= "1010";
    wait for 1 ns;
    assert z = 'X' severity failure;

    wait;
  end process;
end behav;
