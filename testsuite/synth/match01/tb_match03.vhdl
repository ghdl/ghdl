entity tb_match03 is
end tb_match03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_match03 is
  signal a : std_logic_vector(39 downto 0);
  signal z : std_logic;
begin
  dut: entity work.match03
    port map (a, z);

  process
  begin
    a <= x"4fedcba981";
    wait for 1 ns;
    assert z = '1' severity failure;

    a <= x"4fedcba982";
    wait for 1 ns;
    assert z = '0' severity failure;

    a <= x"6fedcba981";
    wait for 1 ns;
    assert z = '0' severity failure;

    a <= x"5fedcba981";
    wait for 1 ns;
    assert z = '1' severity failure;

    wait;
  end process;
end behav;
