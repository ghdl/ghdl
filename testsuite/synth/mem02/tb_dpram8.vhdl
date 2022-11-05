entity tb_dpram8 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dpram8 is
  signal raddr : std_logic_vector(3 downto 0);
  signal rdat : std_logic_vector(7 downto 0);
  signal waddr1 : std_logic_vector(3 downto 0);
  signal wdat1 : std_logic_vector(7 downto 0);
  signal waddr2 : std_logic_vector(3 downto 0);
  signal wdat2 : std_logic_vector(7 downto 0);
  signal rclk, wclk1, wclk2 : std_logic;
begin
  dut: entity work.dpram8
    port map (raddr => raddr, rdat => rdat,
              waddr1 => waddr1, wdat1 => wdat1,
              waddr2 => waddr2, wdat2 => wdat2,
              rclk => rclk, wclk1 => wclk1, wclk2 => wclk2);

  process
    procedure rpulse is
    begin
      rclk <= '0';
      wait for 1 ns;
      rclk <= '1';
      wait for 1 ns;
    end rpulse;

    procedure wpulse1 is
    begin
      wclk1 <= '0';
      wait for 1 ns;
      wclk1 <= '1';
      wait for 1 ns;
    end wpulse1;

    procedure wpulse2 is
    begin
      wclk2 <= '0';
      wait for 1 ns;
      wclk2 <= '1';
      wait for 1 ns;
    end wpulse2;
  begin
    --  Test write port 1
    raddr <= "0000";
    waddr1 <= "0001";
    wdat1 <= x"01";
    wpulse1;

    raddr <= "0001";
    rpulse;
    assert rdat = x"01" severity failure;

    --  Test write port 2.
    waddr2 <= "0010";
    wdat2 <= x"02";
    wdat1 <= x"ff";
    wpulse2;

    --  Check write port 2 doesn't affect read port.
    assert rdat = x"01" severity failure;

    --  Check write port 2 action.
    raddr <= "0010";
    rpulse;
    assert rdat = x"02" severity failure;

    --  Check no change for write port 1.
    raddr <= "0001";
    rpulse;
    assert rdat = x"01" severity failure;

    --  Check write port 1 doesn't affect read port.
    waddr1 <= "0011";
    wdat1 <= x"03";
    raddr <= "0001";
    wdat2 <= x"ff";
    wpulse1;
    assert rdat = x"01" severity failure;

    raddr <= "0011";
    rpulse;
    assert rdat = x"03" severity failure;

    --  And write port 1 doesn't affect write port 2.
    raddr <= "0010";
    rpulse;
    assert rdat = x"02" severity failure;

    wait;
  end process;
end behav;
