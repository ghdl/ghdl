library ieee;
use ieee.std_logic_1164.all;

entity tb_initport is
end;

architecture arch of tb_initport is
  signal top_di : std_logic_vector(7 downto 0);
  signal top_do : std_logic_vector(9 downto 0);
begin
  dut: entity work.initport
    port map (
      top_di => top_di,
      top_do => top_do);

  process
  begin
    top_di <= (others => '1');
    wait for 1 ns;
    assert top_do = b"00_11111111" severity failure;
    wait;
  end process;
end;

