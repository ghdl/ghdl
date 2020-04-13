entity tb_top is
end tb_top;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_top is
  signal clk : std_logic;
  signal addr : std_logic_vector (1 downto 0);
  signal data : std_logic_vector (2 downto 0);
begin
  dut: entity work.top
    port map (clk, addr, data);

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

    addr <= "00";
    pulse;
    assert data = "001" severity failure;
    
    addr <= "10";
    pulse;
    assert data = "100" severity failure;
    
    wait;
  end process;
end behav;
