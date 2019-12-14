entity tb_dff10 is
end tb_dff10;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff10 is
  signal clk : std_logic;
  signal rst : std_logic;
  signal din : std_logic_vector (7 downto 0);
  signal dout : std_logic_vector (7 downto 0);
  signal en : std_logic;
begin
  dut: entity work.dff10
    port map (
      q => dout,
      d => din,
      clk => clk,
      rst => rst,
      en => en);

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
    en <= '1';
    wait for 1 ns;
    assert dout = x"55" severity failure;
    rst <= '0';
    din <= x"7e";
    pulse;
    assert dout = x"7e" severity failure;
    wait;
  end process;
end behav;
