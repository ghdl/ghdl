entity tb_srom02 is
end tb_srom02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_srom02 is
  signal addr : std_logic_vector(3 downto 0);
  signal rdat : std_logic_vector(7 downto 0);
  signal clk : std_logic;
begin
  dut: entity work.srom02
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
    addr <= "0001";
    pulse;
    assert rdat = x"3f" severity failure;

    addr <= "0010";
    pulse;
    assert rdat = x"1e" severity failure;

    addr <= "0100";
    pulse;
    assert rdat = x"3c" severity failure;

    wait;
  end process;
end behav;
