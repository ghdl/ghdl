entity tb_dpram7 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dpram7 is
  signal raddr : std_logic_vector(3 downto 0);
  signal rdat : std_logic_vector(7 downto 0);
  signal waddr1 : std_logic_vector(3 downto 0);
  signal wdat1 : std_logic_vector(7 downto 0);
  signal waddr2 : std_logic_vector(3 downto 0);
  signal wdat2 : std_logic_vector(7 downto 0);
  signal rclk, wclk : std_logic;
begin
  dut: entity work.dpram7
    port map (raddr => raddr, rdat => rdat,
              waddr1 => waddr1, wdat1 => wdat1,
              waddr2 => waddr2, wdat2 => wdat2,
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
    waddr1 <= "0001";
    waddr2 <= "0010";
    wdat1 <= x"01";
    wdat2 <= x"02";
    wpulse;

    raddr <= "0001";
    rpulse;
    assert rdat = x"01" severity failure;

    raddr <= "0010";
    rpulse;
    assert rdat = x"02" severity failure;

    waddr1 <= "0011";
    wdat1 <= x"03";
    raddr <= "0001";
    wpulse;
    assert rdat = x"02" severity failure;

    raddr <= "0011";
    rpulse;
    assert rdat = x"03" severity failure;

    wait;
  end process;
end behav;
