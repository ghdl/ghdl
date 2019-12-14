entity tb_dff04 is
end tb_dff04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff04 is
  signal clk : std_logic;
  signal din : std_logic_vector (7 downto 0);
  signal dout : std_logic_vector (7 downto 0);
begin
  dut: entity work.dff04
    port map (
      r => dout,
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
    assert dout = x"01" severity failure;
    din <= x"ab";
    pulse;
    assert dout = x"ac" severity failure;
    pulse;
    assert dout = x"ac" severity failure;
    din <= x"12";
    pulse;
    assert dout = x"13" severity failure;
    wait;
  end process;
end behav;
