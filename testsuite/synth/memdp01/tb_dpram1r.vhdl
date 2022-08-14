entity tb_dpram1r is
end tb_dpram1r;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dpram1r is
  signal raddr : natural range 0 to 3;
  signal rbit : natural range 0 to 7;
  signal rdat : std_logic;
  signal waddr : natural range 0 to 3;
  signal wdat : std_logic_vector(7 downto 0);
  signal clk : std_logic;
begin
  dut: entity work.dpram1r
    port map (raddr => raddr, rbit => rbit, rdat => rdat,
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
    rbit <= 0;
    waddr <= 1;
    wdat <= x"e1";
    pulse;

    raddr <= 1;
    rbit <= 0;
    waddr <= 0;
    wdat <= x"f0";
    pulse;
    assert rdat = '1' severity failure;

    raddr <= 1;
    rbit <= 1;
    waddr <= 2;
    wdat <= x"d2";
    pulse;
    assert rdat = '0' severity failure;

    raddr <= 1;
    rbit <= 7;
    waddr <= 3;
    wdat <= x"c3";
    pulse;
    assert rdat = '1' severity failure;

    raddr <= 3;
    rbit <= 7;
    waddr <= 0;
    wdat <= x"f0";
    pulse;
    assert rdat = '1' severity failure;

    raddr <= 3;
    rbit <= 5;
    waddr <= 0;
    wdat <= x"f0";
    pulse;
    assert rdat = '0' severity failure;

    wait;
  end process;
end behav;
