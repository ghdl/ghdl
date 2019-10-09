entity tb_forgen02 is
end tb_forgen02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_forgen02 is
  signal a : std_logic_vector (7 downto 0);
begin
  dut: entity work.forgen02
    port map (a);

  process
  begin
    wait for 1 ns;
    assert a = x"00" severity failure;
    wait;
  end process;
end behav;
