entity tb_srom01 is
end tb_srom01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_srom01 is
  signal addr : std_logic_vector(3 downto 0);
  signal rdat : std_logic_vector(7 downto 0);
  signal clk : std_logic;
begin
  dut: entity work.srom01
    port map (clk_i => clk, addr_i => addr, data_o => rdat);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    addr <= "0000";
    pulse;
    assert rdat = x"f0" severity failure;

    addr <= "0001";
    pulse;
    assert rdat = x"e1" severity failure;

    addr <= "0100";
    pulse;
    assert rdat = x"b4" severity failure;

    wait;
  end process;
end behav;
