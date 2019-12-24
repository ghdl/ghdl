entity tb_test is
  generic(
    ROW_BITS : integer := 4;
    WIDTH    : integer := 64
    );
end tb_test;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test is
  signal clk     : std_logic;
  signal rd_addr : std_logic_vector(ROW_BITS - 1 downto 0);
  signal rd_data : std_logic_vector(WIDTH - 1 downto 0);
  signal wr_en   : std_logic;
  signal wr_sel  : std_logic_vector(WIDTH/8 - 1 downto 0);
  signal wr_addr : std_logic_vector(ROW_BITS - 1 downto 0);
  signal wr_data : std_logic_vector(WIDTH - 1 downto 0);
begin
  dut: entity work.test
    generic map (
      ROW_BITS => ROW_BITS,
      WIDTH    => WIDTH)
    port map (
      clk     => clk,
      rd_addr => rd_addr,
      rd_data => rd_data,
      wr_en   => wr_en,
      wr_sel  => wr_sel,
      wr_addr => wr_addr,
      wr_data => wr_data);
  
  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rd_addr <= x"0";
    wr_addr <= x"0";
    wr_data <= x"01_23_45_67_89_ab_cd_ef";
    wr_sel <= x"ff";
    wr_en <= '1';
    pulse;

    --  Simple read.
    rd_addr <= x"0";
    --  And write at a different address.
    wr_addr <= x"1";
    wr_data <= x"ff_ee_dd_cc_bb_aa_99_88";
    wr_en <= '1';
    wr_sel <= x"ff";
    pulse;
    assert rd_data = x"01_23_45_67_89_ab_cd_ef" severity failure;

    rd_addr <= x"1";
    --  Partial write
    wr_addr <= x"0";
    wr_data <= x"00_ee_00_00_00_00_00_00";
    wr_sel <= x"40";
    pulse;
    assert rd_data = x"ff_ee_dd_cc_bb_aa_99_88" severity failure;

    -- Check result.
    rd_addr <= x"0";
    wr_en <= '0';
    pulse;
    assert rd_data = x"01_ee_45_67_89_ab_cd_ef" severity failure;

    --  Check that read is synchronous with clock.
    rd_addr <= x"1";
    assert rd_data = x"01_ee_45_67_89_ab_cd_ef" severity failure;

    --  Check that read occurs before write.
    wr_addr <= x"1";
    wr_data <= x"f0_00_00_00_00_00_00_00";
    wr_sel <= x"80";
    rd_addr <= x"1";
    wr_en <= '1';
    pulse;
    assert rd_data = x"ff_ee_dd_cc_bb_aa_99_88" severity failure;

    wr_en <= '0';
    wr_data <= x"00_00_00_00_00_00_00_00";
    wr_sel <= x"ff";
    pulse;
    assert rd_data = x"f0_ee_dd_cc_bb_aa_99_88" severity failure;

    pulse;
    assert rd_data = x"f0_ee_dd_cc_bb_aa_99_88" severity failure;
    
    wait;
  end process;
end behav;
