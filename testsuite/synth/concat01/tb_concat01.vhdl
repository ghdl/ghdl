entity tb_concat01 is
end tb_concat01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_concat01 is
  signal a : std_logic;
  signal b : std_logic;
  signal z : std_logic_vector(1 downto 0);
begin
  dut: entity work.concat01
    port map (a, b, z);

  process
  begin
    a <= '0';
    b <= '1';
    wait for 1 ns;
    assert z = "01" severity failure;

    a <= '1';
    b <= '1';
    wait for 1 ns;
    assert z = "11" severity failure;

    a <= '1';
    b <= '0';
    wait for 1 ns;
    assert z = "10" severity failure;

    wait;
  end process;
end behav;
