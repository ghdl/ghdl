--Standard Library
library ieee;
--Standard Packages
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_simple_fsm is
end tb_simple_fsm;

architecture tb of tb_simple_fsm is

  signal clk     : std_logic := '0';
  signal rst     : std_logic := '0';
  signal valid   : std_logic := '0';
  signal invalid : std_logic := '0';


  -- Simulation support
  signal clock_ena      : boolean := false;
  constant C_CLK_PERIOD : time    := 10 ns;

-------------------------------------------------------------------------------
-- Clock generator procedure
-------------------------------------------------------------------------------
  procedure clock_gen(
    signal clock_signal   : inout std_logic;
    signal clock_ena      : in    boolean;
    constant clock_period : in    time
    ) is
    variable v_first_half_clk_period : time := clock_period / 2;
  begin
    loop
      if not clock_ena then
        wait until clock_ena;
      end if;
      wait for v_first_half_clk_period;
      clock_signal <= not clock_signal;
      wait for (clock_period - v_first_half_clk_period);
      clock_signal <= not clock_signal;
    end loop;
  end;


begin

  clock_gen(clk, clock_ena, C_CLK_PERIOD);

  -----------------------------------------------------------------------------
  -- DUT
  -----------------------------------------------------------------------------
  dut : entity work.simple_fsm
    port map (
      clk     => clk,
      rst     => rst,
      valid   => valid,
      invalid => invalid);

  p_main : process
  begin
    wait for 100 ns;
    rst       <= '1';
    wait for 100 ns;
    clock_ena <= true;
    wait for 100 ns;
    valid     <= '1';
    wait for 100 ns;
    valid     <= '0';
    wait for 100 ns;
    invalid   <= '1';
    wait for 100 ns;
    invalid   <= '0';
    wait for 100 ns;


    rst <= '0';

    wait for 100 ns;
    valid   <= '1';
    wait for 100 ns;
    valid   <= '0';
    wait for 100 ns;
    invalid <= '1';
    wait for 100 ns;
    invalid <= '0';
    wait for 100 ns;

    -- end sim
    clock_ena <= false;
    wait;

  end process;
end tb;
