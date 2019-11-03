entity tb_sram01 is
end tb_sram01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_sram01 is
  signal addr : std_logic_vector(3 downto 0);
  signal rdat : std_logic_vector(7 downto 0);
  signal wdat : std_logic_vector(7 downto 0);
  signal wen : std_logic;
  signal clk : std_logic;
begin
  dut: entity work.sram01
    port map (clk_i => clk, addr_i => addr, data_i => wdat, data_o => rdat,
              wen_i => wen);

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
    wdat <= x"01";
    wen <= '1';
    pulse;

    addr <= "0001";
    wdat <= x"02";
    pulse;

    addr <= "0000";
    wen <= '0';
    pulse;
    assert rdat = x"01" severity failure;

    wait;
  end process;
end behav;
