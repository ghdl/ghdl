entity tb_insert02 is
end tb_insert02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_insert02 is
  signal a : std_logic_vector (3 downto 0);
  signal b : std_logic_vector (1 downto 0);
  signal o0, o1, o2 : std_logic_vector (3 downto 0);
begin
  dut: entity work.insert02
    port map (a, b, o0, o1, o2);

  process
  begin
    a <= "0111";
    b <= "10";
    wait for 1 ns;
    assert o0 = "0110" severity failure;
    assert o1 = "0101" severity failure;
    assert o2 = "1011" severity failure;
    wait;
  end process;
end behav;
