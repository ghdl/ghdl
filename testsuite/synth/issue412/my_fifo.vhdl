library ieee;
use ieee.std_logic_1164.all;

entity my_FIFO is
  port (
    clk       : in    std_ulogic;
    rst       : in    std_ulogic;
    data_in   : in    std_logic_vector(7 downto 0);
    valid_in  : in    std_ulogic;
    ready_out : out   std_ulogic;
    data_out  : out   std_logic_vector(7 downto 0);
    valid_out : out   std_ulogic;
    ready_in  : in    std_ulogic);
end entity my_FIFO;

architecture behav of my_fifo is
begin
  inst: entity work.generic_sfifo
    generic map (
      t => std_logic_vector (7 downto 0),
      min_depth => 8)
    port map (
      clk => clk,
      rst => rst,
      data_in => data_in,
      valid_in => valid_in,
      ready_out => ready_out,
      data_out => data_out,
      valid_out => valid_out,
      ready_in => ready_in);
end behav;
