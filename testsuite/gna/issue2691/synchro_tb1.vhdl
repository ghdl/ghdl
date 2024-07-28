--==================================================================================================
--! @brief Test bench for the `synchro` module
--!
--!  Makes very basic checks synchronizing a vector of bits.
--!  TODO:
--!    - Add single bit test
--!    - Add custom type test
--!
--! @author Mitja Vodnik <mitja.vodnik@cern.ch> (CERN - EP-LBC)
--! @date 06-05-2024 Created
--! @version 0.1
--! @copyright CERN 2024
--! **License**: CERN-OHL-W-v2
--==================================================================================================

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library colibri;

----------------------------------------------------------------------------------------------------

entity synchro_tb is
  generic (
    g_CLK_PERIOD : time     := 10 ns;
    g_DWIDTH     : positive := 32;
    g_NUM_STAGES : positive := 2
  );
end entity synchro_tb;

----------------------------------------------------------------------------------------------------

architecture behavioural of synchro_tb is

  subtype data_t is std_logic_vector(g_DWIDTH - 1 downto 0);

  -- UUT generics
  constant c_INIT_VALUE : data_t := (others => '1');

  -- UUT signals
  signal clk_i   : std_logic := '1';
  signal reset_i : std_logic := '0';
  signal data_i  : data_t    := not c_INIT_VALUE;
  signal data_o  : data_t;

begin

  clk_i <= not clk_i after g_CLK_PERIOD / 2;
  -- Generate clock and reset

  ----------------------------------------------------------------------------
  --! Unit under test
  ----------------------------------------------------------------------------
  u_synchro : entity colibri.synchro
    generic map (
      DATA_T       => data_t,
      g_INIT_VALUE => c_INIT_VALUE,
      g_NUM_STAGES => g_NUM_STAGES
    )
    port map (
      clk_i   => clk_i,
      reset_i => reset_i,
      data_i  => data_i,
      data_o  => data_o
    );
end architecture behavioural;

----------------------------------------------------------------------------------------------------
