entity tb_top is
end tb_top;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_top is
  signal clk : std_logic;
  signal addr : std_logic_vector(3 downto 0);
  signal dat : std_logic_vector(7 downto 0);
begin
  dut: entity work.top
    port map (clk, dat);

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
    assert dat = x"00" severity failure;

    pulse;
    assert dat = x"01" severity failure;
    
    pulse;
    assert dat = x"02" severity failure;
    
    pulse;
    assert dat = x"03" severity failure;
    
    wait;
  end process;
end behav;
