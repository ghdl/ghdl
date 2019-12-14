entity tb_insert01 is
end tb_insert01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_insert01 is
  signal a : std_logic_vector (3 downto 0);
  signal b : std_logic;
  signal o0, o1, o2, o3 : std_logic_vector (3 downto 0);
begin
  dut: entity work.insert01
    port map (a, b, o0, o1, o2, o3);

  process
  begin
    a <= "0111";
    b <= '0';
    wait for 1 ns;
    assert o0 = "0110" severity failure;
    assert o1 = "0101" severity failure;
    assert o2 = "0011" severity failure;
    assert o3 = "0111" severity failure;
    wait;
  end process;
end behav;
