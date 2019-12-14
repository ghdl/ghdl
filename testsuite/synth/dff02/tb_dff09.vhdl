entity tb_dff09 is
end tb_dff09;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff09 is
  signal clk : std_logic;
  signal rst : std_logic;
  signal din : std_logic_vector (3 downto 0);
  signal dout : std_logic_vector (3 downto 0);
begin
  dut: entity work.dff09
    port map (
      q => dout,
      d => din,
      clk => clk,
      rst => rst);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rst <= '1';
    pulse;
    assert dout = x"0" severity failure;

    rst <= '0';
    din <= x"3";
    pulse;
    assert dout = x"3" severity failure;

    din <= x"a";
    pulse;
    assert dout = x"a" severity failure;

    rst <= '1';
    din <= x"5";
    pulse;
    assert dout = x"0" severity failure;

    wait;
  end process;
end behav;
