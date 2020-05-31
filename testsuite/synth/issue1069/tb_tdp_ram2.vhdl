entity tb_tdp_ram2 is
end tb_tdp_ram2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_tdp_ram2 is
  constant WIDTH_A : natural := 8;
  constant ADDRWIDTH_A : natural := 12;
  
  constant WIDTH_B : natural := 32;
  constant ADDRWIDTH_B : natural := 10;

  constant COL_WIDTH : natural := 8;
  
  signal clk_a        : std_logic;
  signal read_a       : std_logic;
  signal write_a      : std_logic;
  signal byteen_a     : std_logic_vector(WIDTH_A/COL_WIDTH - 1 downto 0);
  signal addr_a       : std_logic_vector(ADDRWIDTH_A - 1 downto 0);
  signal data_read_a  : std_logic_vector(WIDTH_A - 1 downto 0);
  signal data_write_a : std_logic_vector(WIDTH_A - 1 downto 0);
  signal clk_b        : std_logic;
  signal read_b       : std_logic;
  signal write_b      : std_logic;
  signal byteen_b     : std_logic_vector(WIDTH_B/COL_WIDTH - 1 downto 0);
  signal addr_b       : std_logic_vector(ADDRWIDTH_B - 1 downto 0);
  signal data_read_b  : std_logic_vector(WIDTH_B - 1 downto 0);
  signal data_write_b : std_logic_vector(WIDTH_B - 1 downto 0);
begin
  tdp_ram_2: entity work.tdp_ram2
    generic map (
      ADDRWIDTH_A => ADDRWIDTH_A,
      WIDTH_A     => WIDTH_A,
      ADDRWIDTH_B => ADDRWIDTH_B,
      WIDTH_B     => WIDTH_B,
      COL_WIDTH   => COL_WIDTH)
    port map (
      clk_a        => clk_a,
      read_a       => read_a,
      write_a      => write_a,
      byteen_a     => byteen_a,
      addr_a       => addr_a,
      data_read_a  => data_read_a,
      data_write_a => data_write_a,
      clk_b        => clk_b,
      read_b       => read_b,
      write_b      => write_b,
      byteen_b     => byteen_b,
      addr_b       => addr_b,
      data_read_b  => data_read_b,
      data_write_b => data_write_b);

  process
    procedure pulsea is
    begin
      clk_a <= '0';
      wait for 1 ns;
      clk_a <= '1';
      wait for 1 ns;
    end pulsea;

    procedure pulseb is
    begin
      clk_b <= '0';
      wait for 1 ns;
      clk_b <= '1';
      wait for 1 ns;
    end pulseb;
  begin
    clk_a <= '0';
    clk_b <= '0';

    write_b <= '1';
    read_b <= '0';
    addr_b <= b"00_0000_0000";
    byteen_b <= "1111";
    data_write_b <= x"d3_c2_b1_a0";
    pulseb;

    write_a <= '0';
    read_a <= '1';
    addr_a <= x"001";
    pulsea;
    pulsea;
    assert data_read_a = x"b1" severity failure;

    write_a <= '1';
    read_a <= '0';
    byteen_a <= "1";
    addr_a <= x"000";
    data_write_a <= x"10";
    pulsea;

    write_b <= '0';
    read_b <= '1';
    addr_b <= b"00_0000_0000";
    pulseb;
    pulseb;
    assert data_read_b = x"d3_c2_b1_10" severity failure;

    wait;
  end process;
end behav;
