entity tb_attr03 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_attr03 is
  signal rst, clk : std_logic;
  signal cnt : std_logic_vector(7 downto 0);
begin
  dut: entity work.attr03
    port map (rst, clk, cnt);

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

    assert cnt = x"04" severity failure;
    
    wait;
  end process;
end behav;
