entity tb_ram4 is
end tb_ram4;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ram4 is
  constant WIDTH : natural := 8;
  constant ADDRWIDTH : natural := 12;
  
  signal clk_a        : std_logic;
  signal read_a       : std_logic;
  signal write_a      : std_logic;
  signal addr_a       : std_logic_vector(ADDRWIDTH - 1 downto 0);
  signal data_read_a  : std_logic_vector(WIDTH - 1 downto 0);
  signal data_write_a : std_logic_vector(WIDTH - 1 downto 0);
  signal clk_b        : std_logic;
  signal read_b       : std_logic;
  signal write_b      : std_logic;
  signal addr_b       : std_logic_vector(ADDRWIDTH - 1 downto 0);
  signal data_read_b  : std_logic_vector(WIDTH - 1 downto 0);
  signal data_write_b : std_logic_vector(WIDTH - 1 downto 0);
begin
  tdp_ram_1: entity work.ram4
    generic map (
      ADDRWIDTH => ADDRWIDTH,
      WIDTH     => WIDTH)
    port map (
      clk_a        => clk_a,
      read_a       => read_a,
      write_a      => write_a,
      addr_a       => addr_a,
      data_read_a  => data_read_a,
      data_write_a => data_write_a,
      clk_b        => clk_b,
      read_b       => read_b,
      write_b      => write_b,
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

    write_a <= '1';
    read_a <= '0';
    addr_a <= x"000";
    data_write_a <= x"a0";
    pulsea;

    write_b <= '1';
    read_b <= '0';
    addr_b <= x"001";
    data_write_b <= x"b1";
    pulseb;

    write_a <= '0';
    read_a <= '1';
    addr_a <= x"001";
    pulsea;
    assert data_read_a = x"b1" severity failure;

    write_b <= '0';
    read_b <= '1';
    addr_b <= x"000";
    pulseb;
    assert data_read_b = x"a0" severity failure;

    wait;
  end process;
end behav;
