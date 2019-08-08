entity tb_targ01 is
end tb_targ01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_targ01 is
  signal v : std_logic_vector(2 downto 0);
  signal o0 : std_logic;
  signal o1 : std_logic;
  signal o2 : std_logic;
begin
  dut: entity work.targ01
    port map (v, o0, o1, o2);

  process
  begin
    v <= "010";
    wait for 1 ns;
    assert o2 = '0' and o1 = '1' and o0 = '0' severity failure;

    v <= "101";
    wait for 1 ns;
    assert o2 = '1' and o1 = '0' and o0 = '1' severity failure;

    wait;
  end process;
end behav;
