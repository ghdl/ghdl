entity tb_memmux07 is
end tb_memmux07;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_memmux07 is
  signal ad : std_logic;
  signal val : std_logic_vector (1 downto 0);
  signal dat, res : std_logic_vector (7 downto 0);
begin
  dut : entity work.memmux07
    port map (
      ad => ad,
      val => val,
      dat => dat,
      res => res);

  process
  begin
    dat <= x"de";

    ad <= '0';
    val <= "00";
    wait for 1 ns;
    assert res = x"dc" severity failure;

    ad <= '1';
    val <= "00";
    wait for 1 ns;
    assert res = x"ce" severity failure;

    ad <= '0';
    val <= "01";
    wait for 1 ns;
    assert res = x"dd" severity failure;

    ad <= '0';
    val <= "10";
    wait for 1 ns;
    assert res = x"de" severity failure;

    ad <= '1';
    val <= "10";
    wait for 1 ns;
    assert res = x"ee" severity failure;

    wait;
  end process;
end behav;
