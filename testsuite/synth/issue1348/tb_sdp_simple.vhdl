entity tb_sdp_simple is
end tb_sdp_simple;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_sdp_simple is
  signal clk          : std_logic;
  
  signal read_a       : std_logic;
  signal write_a      : std_logic;
  signal byteen_a     : std_logic_vector(0 downto 0);
  signal addr_a       : std_logic_vector(11 downto 0);
  signal data_read_a  : std_logic_vector(7 downto 0);
  signal data_write_a : std_logic_vector(7 downto 0);

  signal read_b       : std_logic;
  signal write_b      : std_logic;
  signal byteen_b     : std_logic_vector(3 downto 0);
  signal addr_b       : std_logic_vector(9 downto 0);
  signal data_read_b  : std_logic_vector(31 downto 0);
  signal data_write_b : std_logic_vector(31 downto 0);
begin
  sdp_simple_2: entity work.sdp_simple
    port map (
      clk          => clk,
      read_a       => read_a,
      write_a      => write_a,
      byteen_a     => byteen_a,
      addr_a       => addr_a,
      data_read_a  => data_read_a,
      data_write_a => data_write_a,
      read_b       => read_b,
      write_b      => write_b,
      byteen_b     => byteen_b,
      addr_b       => addr_b,
      data_read_b  => data_read_b,
      data_write_b => data_write_b);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    clk <= '0';
    write_a <= '0';

    write_b <= '1';
    read_b <= '0';
    addr_b <= b"00_0000_0000";
    byteen_b <= "1111";
    data_write_b <= x"d3_c2_b1_a0";
    pulse;

    write_b <= '0';
    write_a <= '0';
    read_a <= '1';
    addr_a <= x"001";
    pulse;
    assert data_read_a = x"b1" severity failure;

    write_b <= '0';
    write_a <= '1';
    read_a <= '0';
    byteen_a <= "1";
    addr_a <= x"000";
    data_write_a <= x"10";
    pulse;

    write_a <= '0';
    write_b <= '0';
    read_b <= '1';
    addr_b <= b"00_0000_0000";
    pulse;
    assert data_read_b = x"d3_c2_b1_10" severity failure;

    wait;
  end process;
end behav;
