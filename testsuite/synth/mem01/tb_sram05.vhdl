entity tb_sram05 is
end tb_sram05;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_sram05 is
  signal rst : std_logic;
  signal addr : std_logic_vector(3 downto 0);
  signal rdat : std_logic_vector(7 downto 0);
  signal wdat : std_logic_vector(7 downto 0);
  signal wen : std_logic;
  signal clk : std_logic;
begin
  dut: entity work.sram05
    port map (rst => rst, clk_i => clk,
              addr_i => addr, data_i => wdat, data_o => rdat,
              wen_i => wen);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rst <= '0';

    --  [0] := x03
    addr <= "0000";
    wdat <= x"03";
    wen <= '1';
    pulse;
    assert rdat = x"03" severity failure;

    --  [0] := x41
    wdat <= x"41";
    pulse;
    assert rdat = x"41" severity failure;

    --  [4] := x07
    addr <= "0100";
    wdat <= x"07";
    wait for 1 ns;
    pulse;
    assert rdat = x"07" severity failure;

    --  Not en.
    addr <= "0000";
    wen <= '0';
    pulse;
    assert rdat = x"41" severity failure;

    --  [4] := x23
    wen <= '1';
    addr <= "0100";
    wdat <= x"23";
    wait for 1 ns;
    pulse;
    assert rdat = x"23" severity failure;

    --  Reset
    rst <= '1';
    addr <= "0100";
    wdat <= x"ff";
    wait for 1 ns;
    pulse;
    assert rdat = x"23" severity failure;

    --  None
    rst <= '0';
    wen <= '0';
    addr <= "0000";
    wdat <= x"c5";
    pulse;
    assert rdat = x"41" severity failure;

    --  None
    addr <= "0100";
    pulse;
    assert rdat = x"23" severity failure;

    wait;
  end process;
end behav;
