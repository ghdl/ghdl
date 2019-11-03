entity tb_memmux04 is
end tb_memmux04;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_memmux04 is
  signal ad : std_logic_vector (1 downto 0);
  signal val : std_logic;
  signal dat, res : std_logic_vector (3 downto 0);
begin
  dut : entity work.memmux04
    port map (
      ad => ad,
      val => val,
      dat => dat,
      res => res);

  process
  begin
    dat <= x"e";
    
    ad <= "00";
    val <= '0';
    wait for 1 ns;
    assert res = x"e" severity failure;

    ad <= "01";
    val <= '0';
    wait for 1 ns;
    assert res = x"c" severity failure;

    ad <= "00";
    val <= '1';
    wait for 1 ns;
    assert res = x"f" severity failure;

    ad <= "10";
    val <= '0';
    wait for 1 ns;
    assert res = x"a" severity failure;

    ad <= "11";
    val <= '0';
    wait for 1 ns;
    assert res = x"6" severity failure;

    wait;
  end process;
end behav;
