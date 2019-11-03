entity tb_memmux05 is
end tb_memmux05;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_memmux05 is
  signal ad : std_logic;
  signal val : std_logic_vector (1 downto 0);
  signal dat, res : std_logic_vector (2 downto 0);
begin
  dut : entity work.memmux05
    port map (
      ad => ad,
      val => val,
      dat => dat,
      res => res);

  process
  begin
    dat <= "110";

    ad <= '0';
    val <= "00";
    wait for 1 ns;
    assert res = "100" report "1) res=" & to_bstring (res) severity failure;

    ad <= '1';
    val <= "00";
    wait for 1 ns;
    assert res = "000" report "2) res=" & to_bstring (res) severity failure;

    ad <= '0';
    val <= "01";
    wait for 1 ns;
    assert res = "101" report "3) res=" & to_bstring (res) severity failure;

    ad <= '0';
    val <= "10";
    wait for 1 ns;
    assert res = "110" report "4) res=" & to_bstring (res) severity failure;

    ad <= '1';
    val <= "10";
    wait for 1 ns;
    assert res = "100" report "5) res=" & to_bstring (res) severity failure;

    dat <= "010";

    ad <= '0';
    val <= "00";
    wait for 1 ns;
    assert res = "000" report "6) res=" & to_bstring (res) severity failure;

    ad <= '1';
    val <= "00";
    wait for 1 ns;
    assert res = "000" report "7) res=" & to_bstring (res) severity failure;

    ad <= '1';
    val <= "10";
    wait for 1 ns;
    assert res = "100" report "8) res=" & to_bstring (res) severity failure;

    wait;
  end process;
end behav;
