entity tb_ram3 is
end tb_ram3;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ram3 is
  signal val : std_logic_vector(7 downto 0);
  signal waddr : std_logic_vector(2 downto 0);
  signal wdat : std_logic;
  signal clk : std_logic;
begin
  dut: entity work.ram3
    port map (waddr => waddr, wdat => wdat, val => val,
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
    waddr <= "000";
    wdat <= '0';
    pulse;
    
    waddr <= "001";
    wdat <= '1';
    pulse;

    assert (val and x"03") = x"02" severity failure;

    wait;
  end process;
end behav;
