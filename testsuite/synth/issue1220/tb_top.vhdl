entity tb_top is
end tb_top;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_top is
  signal clk : std_logic;
  signal x, y : std_logic_vector (1 downto 0);
  signal data : std_logic_vector (3 downto 0);
begin
  dut: entity work.top
    port map (clk, x, y, data);

  process
    procedure pulse is
    begin
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      clk <= '0';
    end pulse;
  begin
    clk <= '0';

    x <= "00";
    y <= "00";
    pulse;
    assert data = "0001" severity failure;

    x <= "10";
    pulse;
    assert data = "1110" severity failure;

    y <= "01";
    pulse;
    assert data = "1101" severity failure;

    x <= "10";
    y <= "11";
    pulse;
    assert data = "0111" severity failure;

    wait;
  end process;
end behav;
