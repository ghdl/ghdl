entity tb_my_entity is
end tb_my_entity;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_my_entity is
  signal a : std_logic_vector(0 downto 0);
begin
  dut: entity work.my_entity
    port map (a);

  process
  begin
    wait for 1 ns;
    assert a = "0" severity failure;
    wait;
  end process;
end behav;
