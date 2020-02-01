entity tb_dff08b is
end tb_dff08b;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff08b is
  signal clk : std_logic;
  signal rst : std_logic;
  signal en : std_logic;
  signal din : std_logic_vector (7 downto 0);
  signal dout : std_logic_vector (7 downto 0);
begin
  dut: entity work.dff08b
    port map (
      q => dout,
      d => din,
      en => en,
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
    wait for 1 ns;
    assert dout = x"aa" severity failure;

    rst <= '1';
    en <= '1';
    pulse;
    assert dout = x"00" severity failure;

    rst <= '0';
    din <= x"38";
    pulse;
    assert dout = x"38" severity failure;

    din <= x"af";
    pulse;
    assert dout = x"af" severity failure;

    en <= '0';
    din <= x"b3";
    pulse;
    assert dout = x"af" severity failure;

    en <= '0';
    rst <= '1';
    din <= x"b4";
    pulse;
    assert dout = x"af" severity failure;

    en <= '1';
    rst <= '1';
    din <= x"b5";
    pulse;
    assert dout = x"00" severity failure;

    wait;
  end process;
end behav;
