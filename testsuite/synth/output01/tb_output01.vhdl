library ieee;
use ieee.std_logic_1164.all;

entity tb_output01 is
end tb_output01;

architecture behav of tb_output01 is
  signal i : std_logic;
  signal o : std_logic_vector (1 downto 0);
begin
  inst: entity work.output01
    port map (i => i, o => o);

  process
  begin
    i <= '0';
    wait for 1 ns;
    assert o = "10" severity failure;

    i <= '1';
    wait for 1 ns;
    assert o = "01" severity failure;

    wait;
  end process;
end behav;

