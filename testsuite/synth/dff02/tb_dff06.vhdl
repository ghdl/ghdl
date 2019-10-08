entity tb_dff06 is
end tb_dff06;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff06 is
  signal clk : std_logic;
  signal rst : std_logic;
  signal din : std_logic_vector (7 downto 0);
  signal dout : std_logic_vector (7 downto 0);
begin
  dut: entity work.dff06
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
    rst <= '0';
    din <= x"7e";
    pulse;
    assert dout = x"7e" severity failure;

    din <= x"38";
    pulse;
    assert dout = x"38" severity failure;

    rst <= '1';
    din <= x"af";
    pulse;
    assert dout = x"38" severity failure;

    rst <= '0';
    pulse;
    assert dout = x"af" severity failure;

    wait;
  end process;
end behav;
