entity tb_dff03 is
end tb_dff03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff03 is
  signal clk : std_logic;
  signal din : std_logic_vector (7 downto 0);
  signal dout : std_logic_vector (7 downto 0);
begin
  dut: entity work.dff03
    port map (
      q => dout,
      d => din,
      clk => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    din <= x"00";
    pulse;
    assert dout = x"00" severity failure;
    din <= x"ab";
    pulse;
    assert dout = x"ab" severity failure;
    pulse;
    assert dout = x"ab" severity failure;
    din <= x"12";
    pulse;
    assert dout = x"12" severity failure;
    wait;
  end process;
end behav;
