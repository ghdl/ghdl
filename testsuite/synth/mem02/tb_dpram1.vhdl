entity tb_dpram1 is
end tb_dpram1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dpram1 is
  signal raddr : std_logic_vector(3 downto 0);
  signal rdat : std_logic_vector(7 downto 0);
  signal waddr : std_logic_vector(3 downto 0);
  signal wdat : std_logic_vector(7 downto 0);
  signal clk : std_logic;
begin
  dut: entity work.dpram1
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
    waddr <= "0001";
    wdat <= x"01";
    pulse;

    raddr <= "0001";
    waddr <= "0010";
    wdat <= x"02";
    pulse;
    assert rdat = x"01" severity failure;

    wait;
  end process;
end behav;
