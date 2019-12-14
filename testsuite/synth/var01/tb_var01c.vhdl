entity tb_var01c is
end tb_var01c;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_var01c is
  signal clk : std_logic;
  signal mask : std_logic_vector (1 downto 0);
  signal val : std_logic_vector (1 downto 0);
  signal res : std_logic_vector (3 downto 0);
begin
  dut: entity work.var01c
    port map (
      clk => clk,
      mask => mask,
      val => val,
      res => res);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    mask <= "11";
    val <= b"01";
    pulse;
    assert res = b"01_01" report "res=" & to_bstring (res) severity failure;

    mask <= "10";
    val <= b"11";
    pulse;
    assert res = b"11_01" severity failure;

    mask <= "00";
    val <= b"00";
    pulse;
    assert res = b"11_01" severity failure;

    mask <= "01";
    val <= b"10";
    pulse;
    assert res = b"11_10" severity failure;

    wait;
  end process;
end behav;
