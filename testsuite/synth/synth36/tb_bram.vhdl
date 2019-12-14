entity tb_bram is
end tb_bram;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_bram is
  signal clk, we : std_logic;
  signal addr : std_logic_vector (8 downto 0);
  signal dout : std_logic_vector (7 downto 0);
  signal din : std_logic_vector (7 downto 0);
begin
  dut: entity work.bram
    port map (clk => clk, we => we, addr => addr,
              data_in => din, data_out => dout);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    we <= '1';
    addr <= "000000001";
    din <= x"f1";
    pulse;

    we <= '0';
    pulse;
    assert dout = x"f1" severity failure;

    we <= '1';
    addr <= "000000011";
    din <= x"f3";
    pulse;

    we <= '1';
    addr <= "111111111";
    din <= x"ff";
    pulse;

    we <= '0';
    addr <= "000000011";
    pulse;
    assert dout = x"f3" severity failure;

    we <= '0';
    addr <= "111111111";
    pulse;
    assert dout = x"ff" severity failure;

    wait;
  end process;
end behav;
