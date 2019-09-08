entity tb_dpram3 is
end tb_dpram3;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dpram3 is
  signal raddr : std_logic_vector(3 downto 0);
  signal rdat : std_logic_vector(7 downto 0);
  signal waddr : std_logic_vector(3 downto 0);
  signal wdat : std_logic_vector(7 downto 0);
  signal clk : std_logic;
begin
  dut: entity work.dpram3
    port map (raddr => raddr, rdat => rdat, waddr => waddr, wdat => wdat,
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
    raddr <= "0000";
    waddr <= x"a";
    wdat <= x"5a";
    pulse;

    raddr <= x"a";
    waddr <= x"7";
    wdat <= x"87";
    pulse;
    assert rdat = x"5a" severity failure;

    raddr <= x"7";
    waddr <= x"1";
    wdat <= x"e1";
    pulse;
    assert rdat = x"87" severity failure;

    raddr <= x"1";
    waddr <= x"3";
    wdat <= x"c3";
    pulse;
    assert rdat = x"e1" severity failure;

    wait;
  end process;
end behav;
