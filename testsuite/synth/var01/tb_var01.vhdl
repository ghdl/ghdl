entity tb_var01 is
end tb_var01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_var01 is
  signal clk : std_logic;
  signal mask : std_logic_vector (3 downto 0);
  signal val : std_logic_vector (31 downto 0);
  signal res : std_logic_vector (31 downto 0);
begin
  dut: entity work.var01
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
    mask <= x"f";
    val <= x"12_34_56_78";
    pulse;
    assert res = x"12_34_56_78" report "res=" & to_hstring (res) severity failure;
    
    mask <= x"8";
    val <= x"9a_00_00_00";
    pulse;
    assert res = x"9a_34_56_78" severity failure;

    mask <= x"0";
    val <= x"00_00_00_00";
    pulse;
    assert res = x"9a_34_56_78" severity failure;

    mask <= x"5";
    val <= x"00_bc_00_de";
    pulse;
    assert res = x"9a_bc_56_de" severity failure;

    wait;
  end process;
end behav;
