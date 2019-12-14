entity tb_memmux01 is
end tb_memmux01;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_memmux01 is
  signal wen  : std_logic;
  signal addr : std_logic_vector (3 downto 0);
  signal wdat : std_logic;
  signal rdat : std_logic_vector (15 downto 0);
  signal clk  : std_logic;
  signal rst  : std_logic;
begin
  dut : entity work.memmux01
    port map (
      wen  => wen,
      addr => addr,
      wdat => wdat,
      rdat => rdat,
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
    variable v : std_logic_vector(15 downto 0);
  begin
    rst <= '1';
    wen <= '0';
    wdat <= '1';
    addr <= x"0";
    pulse;

    rst <= '0';
    addr <= x"4";
    wen <= '1';
    pulse;
    assert rdat = x"0000" severity failure;

    addr <= x"f";
    pulse;
    assert rdat = x"0010" severity failure;

    addr <= x"4";
    wdat <= '0';
    pulse;
    assert rdat = x"8010" severity failure;

    pulse;
    assert rdat = x"8000" severity failure;

    v := x"8000";
    wdat <= '1';
    for i in 0 to 14 loop
      addr <= std_logic_vector(to_unsigned(i, 4));
      pulse;
      assert rdat = v severity failure;
      v (i) := '1';
    end loop;

    wait;
  end process;
end behav;
