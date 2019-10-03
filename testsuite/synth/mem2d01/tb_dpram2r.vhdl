entity tb_dpram2r is
end tb_dpram2r;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dpram2r is
  signal raddr : natural range 0 to 3;
  signal rnib : natural range 0 to 1;
  signal rdat : std_logic_vector (3 downto 0);
  signal waddr : natural range 0 to 3;
  signal wdat : std_logic_vector(7 downto 0);
  signal clk : std_logic;
begin
  dut: entity work.dpram2r
    port map (raddr => raddr, rnib => rnib, rdat => rdat,
              waddr => waddr, wdat => wdat,
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
    raddr <= 0;
    rnib <= 0;
    waddr <= 1;
    wdat <= x"e1";
    pulse;

    raddr <= 1;
    rnib <= 0;
    waddr <= 0;
    wdat <= x"f0";
    pulse;
    assert rdat = x"1" severity failure;

    raddr <= 1;
    rnib <= 1;
    waddr <= 2;
    wdat <= x"d2";
    pulse;
    assert rdat = x"e" severity failure;

    raddr <= 2;
    rnib <= 1;
    waddr <= 3;
    wdat <= x"c3";
    pulse;
    assert rdat = x"d" severity failure;

    raddr <= 3;
    rnib <= 0;
    waddr <= 0;
    wdat <= x"f0";
    pulse;
    assert rdat = x"3" severity failure;

    raddr <= 3;
    rnib <= 1;
    waddr <= 0;
    wdat <= x"f0";
    pulse;
    assert rdat = x"c" severity failure;

    wait;
  end process;
end behav;
