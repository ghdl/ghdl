entity tb_var01a is
end tb_var01a;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_var01a is
  signal clk : std_logic;
  signal mask : std_logic_vector (1 downto 0);
  signal val : std_logic_vector (7 downto 0);
  signal res : std_logic_vector (7 downto 0);
begin
  dut: entity work.var01a
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
    val <= x"12";
    pulse;
    assert res = x"12" report "res=" & to_hstring (res) severity failure;
    
    mask <= "10";
    val <= x"9a";
    pulse;
    assert res = x"92" severity failure;

    mask <= "00";
    val <= x"00";
    pulse;
    assert res = x"92" severity failure;

    mask <= "01";
    val <= x"de";
    pulse;
    assert res = x"9e" severity failure;

    wait;
  end process;
end behav;
