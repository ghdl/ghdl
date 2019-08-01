library ieee;
use ieee.std_logic_1164.all;

entity tb_output06 is
end tb_output06;

architecture behav of tb_output06 is
  signal i : std_logic;
  signal o : std_logic_vector (3 downto 0);
begin
  inst: entity work.output06
    port map (i => i, o => o);

  process
  begin
    i <= '0';
    wait for 1 ns;
    assert o = "0010" severity failure;

    i <= '1';
    wait for 1 ns;
    assert o = "1001" severity failure;

    wait;
  end process;
end behav;

