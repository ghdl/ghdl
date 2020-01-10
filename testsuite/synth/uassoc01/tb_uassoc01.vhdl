entity tb_uassoc01 is
end tb_uassoc01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_uassoc01 is
  signal i1 : std_logic_vector(3 downto 0);
  signal i2 : std_logic_vector(7 downto 0);
  signal o  : std_logic_vector(3 downto 0);
begin
  dut: entity work.uassoc01
    port map (i1, i2, o);

  process
  begin
    i1 <= "1100";
    i2 <= b"1010_1010";
    wait for 1 ns;
    assert o = "0110" severity failure;
    wait;
  end process;
end behav;
