entity tb_targ03 is
end tb_targ03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_targ03 is
  signal rdat : std_logic_vector (7 downto 0);
  signal rv   : std_logic;
  signal wdat : std_logic_vector (7 downto 0);
  signal wval : std_logic;
  signal wen  : std_logic;
  signal clk  : std_logic;
begin
  dut: entity work.targ03
    port map (rdat => rdat, rv => rv,
              wdat => wdat, wval => wval,
              wen => wen, clk => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    wen <= '1';
    wdat <= x"45";
    wval <= '1';
    pulse;
    assert rdat = x"45" severity failure;
    assert rv = '1' severity failure;

    wdat <= x"ca";
    wval <= '0';
    pulse;
    assert rdat = x"ca" severity failure;
    assert rv = '0' severity failure;

    wen <= '0';
    wdat <= x"e3";
    wval <= '1';
    pulse;
    assert rdat = x"ca" severity failure;
    assert rv = '0' severity failure;

    wait;
  end process;
end behav;
