entity tb_memmux02 is
end tb_memmux02;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_memmux02 is
  signal wen  : std_logic;
  signal addr : std_logic_vector (3 downto 0);
  signal rdat : std_logic;
  signal wdat : std_logic_vector (15 downto 0);
  signal clk  : std_logic;
  signal rst  : std_logic;
begin
  dut : entity work.memmux02
    port map (
      wen  => wen,
      addr => addr,
      rdat => rdat,
      wdat => wdat,
      clk  => clk,
      rst  => rst);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;

    constant c : std_logic_vector (15 downto 0) := x"56bc";
  begin
    rst <= '1';
    wen <= '0';
    wdat <= c;
    addr <= x"0";
    pulse;

    rst <= '0';
    pulse;
    assert rdat = '0' severity failure;

    addr <= x"4";
    wen <= '1';
    pulse;
    assert rdat = '0' severity failure;

    wen <= '0';
    pulse;
    assert rdat = '1' severity failure;

    for i in c'range loop
      addr <= std_logic_vector (to_unsigned (i, 4));
      pulse;
      assert rdat = c(i) severity failure;
    end loop;

    wait;
  end process;
end behav;
