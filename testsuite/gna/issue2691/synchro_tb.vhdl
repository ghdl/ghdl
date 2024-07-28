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

library vunit_lib;
  context vunit_lib.vunit_context;

library osvvm;
  context osvvm.osvvmcontext;

----------------------------------------------------------------------------------------------------

entity synchro_tb is
  generic (
    g_CLK_PERIOD : time     := 10 ns;
    g_DWIDTH     : positive := 32;
    g_NUM_STAGES : positive := 2;
    RUNNER_CFG   : string
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

  -- Generate clock and reset
  osvvm.TbUtilPkg.CreateClock(clk_i, g_CLK_PERIOD);

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

  ----------------------------------------------------------------------------
  --! Verification process (VUnit)
  ----------------------------------------------------------------------------
  proc_vunit : process is
  begin

    ------------ VUNIT SETUP ------------
    test_runner_setup(runner, RUNNER_CFG);

    -- TEST: Module is initialized without a reset signal
    wait until rising_edge(clk_i);
    check(data_o = c_INIT_VALUE, "Check that the output is initialized correctly.");

    -- TEST: Input reaches output after it passes all the register stages
    osvvm.TbUtilPkg.WaitForClock(clk_i, g_NUM_STAGES);
    check(data_o = data_i, "Check that the input data reaches the output.");

    -- TEST: The change in input reaches the output after passing all the register stages
    wait for g_CLK_PERIOD / 3;
    data_i <= std_logic_vector(to_unsigned(33, g_DWIDTH));
    osvvm.TbUtilPkg.WaitForClock(clk_i, g_NUM_STAGES);
    check(data_o /= data_i, "Check that the output does not transition until after all the register stages were passed.");
    osvvm.TbUtilPkg.WaitForClock(clk_i, 1);
    check(data_o = data_i, "Check that the input data reaches the output.");

    -- TEST: Reset signal re-initializes the module
    osvvm.TbUtilPkg.WaitForClock(clk_i, 1);
    reset_i <= '1';
    osvvm.TbUtilPkg.WaitForClock(clk_i, 2);
    check(data_o = c_INIT_VALUE, "Check that the output is initialized after the reset.");
    reset_i <= '0';
    osvvm.TbUtilPkg.WaitForClock(clk_i, g_NUM_STAGES + 1);
    check(data_o = data_i, "Check that the input data reaches the output.");

    ----------- VUNIT CLEANUP -----------
    test_runner_cleanup(runner);

  end process proc_vunit;

  test_runner_watchdog(runner, 10 ms);

end architecture behavioural;

----------------------------------------------------------------------------------------------------
