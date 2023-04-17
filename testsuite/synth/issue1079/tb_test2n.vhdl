library ieee;
use ieee.std_logic_1164.all;

entity tb_test2n is
end;

architecture behav of tb_test2n is
  signal clk     : std_logic;
  signal rd_addr : std_logic_vector(7 downto 0);
  signal rd_data : std_logic_vector(63 downto 0);
  signal en   : std_logic;
  signal wr_sel  : std_logic_vector(7 downto 0);
  signal wr_addr : std_logic_vector(7 downto 0);
  signal wr_data : std_logic_vector(63 downto 0);
begin
  inst_test2n: entity work.test2n
    port map (
      clk     => clk,
      en      => en,
      rd_addr => rd_addr,
      rd_data => rd_data,
      wr_sel  => wr_sel,
      wr_addr => wr_addr,
      wr_data => wr_data);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 5 ns;
      clk <= '1';
      wait for 5 ns;
    end pulse;
  begin
    en <= '0';
    wr_sel <= x"ff";
    pulse;

    en <= '1';
    wr_addr <= x"01";
    wr_data <= x"01_12_34_56_78_9a_bc_de";
    pulse;

    wr_addr <= x"02";
    wr_data <= x"02_12_34_56_78_9a_bc_de";

    rd_addr <= x"01";
    pulse;

    assert rd_data = x"01_12_34_56_78_9a_bc_de";
    wait;
  end process;
end behav;
