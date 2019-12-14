entity tb_dpram2w is
end tb_dpram2w;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dpram2w is
  signal waddr : natural range 0 to 3;
  signal wnib : natural range 0 to 1;
  signal wdat : std_logic_vector (3 downto 0);
  signal raddr : natural range 0 to 3;
  signal rdat : std_logic_vector(7 downto 0);
  signal clk : std_logic;
begin
  dut: entity work.dpram2w
    port map (waddr => waddr, wnib => wnib, wdat => wdat,
              raddr => raddr, rdat => rdat,
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
    waddr <= 0;
    wnib <= 0;
    wdat <= x"0";
    raddr <= 1;
    pulse;

    waddr <= 0;
    wnib <= 1;
    wdat <= x"f";
    raddr <= 1;
    pulse;

    waddr <= 1;
    wnib <= 1;
    wdat <= x"e";
    raddr <= 0;
    pulse;
    assert rdat = x"f0" severity failure;

    waddr <= 1;
    wnib <= 0;
    wdat <= x"1";
    raddr <= 0;
    pulse;
    assert rdat = x"f0" severity failure;

    waddr <= 3;
    wnib <= 0;
    wdat <= x"3";
    raddr <= 1;
    pulse;
    assert rdat = x"e1" severity failure;

    waddr <= 3;
    wnib <= 1;
    wdat <= x"c";
    raddr <= 1;
    pulse;
    assert rdat = x"e1" severity failure;

    waddr <= 2;
    wnib <= 1;
    wdat <= x"d";
    raddr <= 3;
    pulse;
    assert rdat = x"c3" severity failure;

    waddr <= 2;
    wnib <= 0;
    wdat <= x"2";
    raddr <= 3;
    pulse;
    assert rdat = x"c3" severity failure;

    waddr <= 1;
    wnib <= 0;
    wdat <= x"1";
    raddr <= 2;
    pulse;
    assert rdat = x"d2" severity failure;

    wait;
  end process;
end behav;
