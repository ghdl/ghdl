entity tb_var01b is
end tb_var01b;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_var01b is
  signal clk : std_logic;
  signal mask : std_logic_vector (1 downto 0);
  signal val : std_logic_vector (3 downto 0);
  signal res : std_logic_vector (3 downto 0);
begin
  dut: entity work.var01b
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
    val <= b"00_01";
    pulse;
    assert res = b"00_01" report "res=" & to_bstring (res) severity failure;

    mask <= "10";
    val <= b"11_00";
    pulse;
    assert res = b"11_01" severity failure;

    mask <= "00";
    val <= b"00_00";
    pulse;
    assert res = b"11_01" severity failure;

    mask <= "01";
    val <= b"00_10";
    pulse;
    assert res = b"11_10" severity failure;

    wait;
  end process;
end behav;
