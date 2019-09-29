entity tb_ram4 is
end tb_ram4;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ram4 is
  signal rdat : std_logic_vector(1 downto 0);
  signal raddr : std_logic_vector(1 downto 0);
  signal init : std_logic_vector(7 downto 0);
  signal rst : std_logic;
  signal clk : std_logic;
begin
  dut: entity work.ram4
    port map (raddr => raddr, rdat => rdat, rst => rst, init => init,
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
    rst <= '1';
    init <= b"01_11_00_10";
    pulse;

    rst <= '0';
    init <= b"00_00_00_00";
    raddr <= "00";
    pulse;
    assert rdat = "10" severity failure;

    raddr <= "11";
    pulse;
    assert rdat = "01" severity failure;

    raddr <= "01";
    pulse;
    assert rdat = "00" severity failure;

    raddr <= "10";
    pulse;
    assert rdat = "11" severity failure;

    wait;
  end process;
end behav;
