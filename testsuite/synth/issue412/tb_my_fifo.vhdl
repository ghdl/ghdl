library ieee;
use ieee.std_logic_1164.all;

entity tb_my_FIFO is
end;

architecture behav of tb_my_fifo is
  signal clk       : std_ulogic;
  signal rst       : std_ulogic;
  signal data_in   : std_logic_vector(7 downto 0);
  signal valid_in  : std_ulogic;
  signal ready_out : std_ulogic;
  signal data_out  : std_logic_vector(7 downto 0);
  signal valid_out : std_ulogic;
  signal ready_in  : std_ulogic;
begin
  inst_my_FIFO: entity work.my_FIFO
    port map (
      clk       => clk,
      rst       => rst,
      data_in   => data_in,
      valid_in  => valid_in,
      ready_out => ready_out,
      data_out  => data_out,
      valid_out => valid_out,
      ready_in  => ready_in);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 5 ns;
      clk <= '1';
      wait for 5 ns;
    end pulse;
  begin
    rst <= '1';
    valid_in <= '0';
    ready_in <= '0';
    pulse;

    rst <= '0';
    pulse;

    assert valid_out = '0' severity failure;
    assert ready_out = '1' severity failure;

    data_in <= x"d5";
    valid_in <= '1';
    pulse;

    valid_in <= '0';

    --  Need a second cycle to see the data on the outputs.
    pulse;
    assert valid_out = '1' severity failure;
    assert data_out = x"d5" severity failure;

    ready_in <= '1';
    pulse;
    assert valid_out = '0' severity failure;

    ready_in <= '0';
    wait;
  end process;
end behav;
