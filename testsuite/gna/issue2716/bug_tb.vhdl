library ieee;
use ieee.std_logic_1164.all;

entity ghdl_bug_tb is
end entity ghdl_bug_tb;

architecture sim of ghdl_bug_tb is
begin

  g_sync_generic : for i in 0 to 1 generate

    i_sync_generic : entity work.sync_generic
      generic map (
        generic_data_type => std_logic,
        GC_RESET_VALUE    => '0'
      )
      port map (
        clk       => '0',
        rst       => '0',
        value_in  => '0',
        value_out => open
      );

  end generate g_sync_generic;


  p_sequencer : process
  begin
    wait for 100 ns;
    report "Simulation done" severity note;
    std.env.finish;
  end process p_sequencer;

end architecture sim;
