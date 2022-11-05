entity tb_dpram4 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dpram4 is
  signal raddr : std_logic_vector(3 downto 0);
  signal rdat : std_logic_vector(7 downto 0);
  signal waddr : std_logic_vector(3 downto 0);
  signal wdat : std_logic_vector(7 downto 0);
  signal rclk, wclk : std_logic;
begin
  dut: entity work.dpram4
    port map (raddr => raddr, rdat => rdat, waddr => waddr, wdat => wdat,
              rclk => rclk, wclk => wclk);

  process
    procedure rpulse is
    begin
      rclk <= '0';
      wait for 1 ns;
      rclk <= '1';
      wait for 1 ns;
    end rpulse;

    procedure wpulse is
    begin
      wclk <= '0';
      wait for 1 ns;
      wclk <= '1';
      wait for 1 ns;
    end wpulse;
  begin
    raddr <= "0000";
    waddr <= "0001";
    wdat <= x"01";
    wpulse;

    raddr <= "0001";
    rpulse;
    assert rdat = x"01" severity failure;

    waddr <= "0010";
    wdat <= x"02";
    wpulse;
    assert rdat = x"01" severity failure;

    wait;
  end process;
end behav;
