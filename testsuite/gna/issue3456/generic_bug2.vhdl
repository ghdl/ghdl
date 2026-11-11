library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity generic_bug is
  generic(
    GC_ADDR_WIDTH : integer := 8
  );
end entity generic_bug;

architecture sim of generic_bug is

  signal clk           : std_logic := '0';
  signal reset         : std_logic := '1';
  signal data_in       : unsigned(GC_ADDR_WIDTH-1 downto 0) := (others=>'0');
  signal data_out      : unsigned(GC_ADDR_WIDTH-1 downto 0);

  subtype gen_type is unsigned(GC_ADDR_WIDTH-1 downto 0);
begin

  i_generic_entity : entity work.generic_entity
    generic map (
      gt_generic_data_type => gen_type,
      GC_RESET_VALUE       => (others=>'0')
    )
    port map (
      clk           => clk,
      reset         => reset,
      input_value   => data_in,
      output_value  => data_out
    );

  p_clk : process
  begin
    loop
      wait for 5 ns;
      clk <= not clk;
    end loop;
  end process p_clk;

  p_data_in : process(clk)
  begin
    if rising_edge(clk) then
      data_in <= data_in + 1;
    end if;
  end process p_data_in;

  p_sequencer : process
  begin
    wait for 100 ns;
    report "End of simulation";
    std.env.finish;
  end process p_sequencer;

end architecture sim;
