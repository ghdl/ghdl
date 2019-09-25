entity tb_lut is
end tb_lut;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_lut is
  signal c : std_logic;
  signal s : std_logic_vector(1 downto 0);
begin
  dut: entity work.lut
    port map (s, c);

  process
  begin
    s <= "00";
    wait for 1 ns;
    assert c = '1' severity failure;

    s <= "01";
    wait for 1 ns;
    assert c = '0' severity failure;

    s <= "10";
    wait for 1 ns;
    assert c = '1' severity failure;

    s <= "11";
    wait for 1 ns;
    assert c = '0' severity failure;

    wait;
  end process;
end behav;
