--Testbench file:

library ieee;
use ieee.std_logic_1164.all;

entity tb_ghdl_bug is
end entity tb_ghdl_bug;

architecture rtl of tb_ghdl_bug is

  signal clk        : std_logic := '0';
  signal reset      : std_logic := '0';
  signal value_in   : std_logic_vector(1 downto 0) := (others=>'0');
  signal value_out  : std_logic_vector(1 downto 0);

begin

  clk <= not clk after 5 ns;

  i_ghdl_bug : entity work.ghdl_bug
    generic map (
      generic_data_type => std_logic_vector(1 downto 0),
      GC_RESET_VALUE => "00"
    )
    port map (
      clk       => clk,
      reset     => reset,
      value_in  => value_in,
      value_out => value_out
    );

  p_sequencer : process
  begin
    wait for 100 ns;

    std.env.finish;
  end process p_sequencer;

end architecture rtl;
