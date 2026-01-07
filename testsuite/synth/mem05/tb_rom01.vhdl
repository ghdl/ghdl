entity tb_rom01 is
end tb_rom01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_rom01 is
  signal addr : std_logic_vector(3 downto 0);
  signal dat : std_logic_vector(14 downto 0);
begin
  dut: entity work.rom01
    port map (addr => addr, dat => dat);

  process
  begin
    addr <= "0000";
    wait for 1 ns;
    assert dat = b"111_101_101_101_111" severity failure;

    addr <= "0010";
    wait for 1 ns;
    assert dat = b"111_001_111_100_111" severity failure;

    addr <= "0111";
    wait for 1 ns;
    assert dat = b"111_001_001_001_001" severity failure;

    wait;
  end process;
end behav;
