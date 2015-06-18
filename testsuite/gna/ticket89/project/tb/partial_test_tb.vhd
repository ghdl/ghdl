--========================================================================================================================
-- Copyright (c) 2015 by Bitvis AS.  All rights reserved.
-- A free license is hereby granted, free of charge, to any person obtaining
-- a copy of this VHDL code and associated documentation files (for 'Bitvis Utility Library'),
-- to use, copy, modify, merge, publish and/or distribute - subject to the following conditions:
--  - This copyright notice shall be included as is in all copies or substantial portions of the code and documentation
--  - The files included in Bitvis Utility Library may only be used as a part of this library as a whole
--  - The License file may not be modified
--  - The calls in the code to the license file ('show_license') may not be removed or modified.
--  - No other conditions whatsoever may be added to those of this License

-- BITVIS UTILITY LIBRARY AND ANY PART THEREOF ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
-- INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH BITVIS UTILITY LIBRARY.
--========================================================================================================================

------------------------------------------------------------------------------------------
-- VHDL unit     : Bitvis Utility Library : partial_test_tb
--
-- Description   : Parts of the testbench used for testing the Bitvis Utility Library
------------------------------------------------------------------------------------------


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library STD;
use std.textio.all;

library work;
use work.types_pkg.all;
use work.string_methods_pkg.all;
use work.adaptations_pkg.all;
use work.methods_pkg.all;
use work.bfm_common_pkg.all;

library ieee_proposed;
use ieee_proposed.standard_additions.all;       -- Used for to_string(real)
use ieee_proposed.std_logic_1164_additions.all; -- Used for to_string(std_logic)


entity partial_test_tb is
end entity;


architecture func of partial_test_tb is

  signal sl       : std_logic                     := '0';
  signal clk100M      : std_logic;
  signal clk100M_ena  : boolean := true;

  signal clk200M      : std_logic;
  signal clk200M_ena  : boolean := true;

  signal clk50M      : std_logic;

  constant C_CLK100M_PERIOD : time := 10 ns;

begin

  ------------------------------------------------
  -- Process: clock generator
  ------------------------------------------------
  -- Overloaded version with enable signal as argument
  clock_generator(clk100M, clk100M_ena, C_CLK100M_PERIOD, "Clk100M");

  ------------------------------------------------
  -- PROCESS: p_main
  ------------------------------------------------
  p_main: process
    constant C_SCOPE     : string  := "TB seq";

    -- Log overloads for simplification
    procedure log(
      msg   : string) is
    begin
      log(ID_SEQUENCER, msg, C_SCOPE);
    end;



  begin
    set_alert_file_name("alertlog.txt");
    set_log_file_name("testlog.txt");


    set_alert_stop_limit(WARNING, 0);
    set_alert_stop_limit(ERROR, 0);    -- 0 = Never stop
    wait for 1 ns;

   
   
    -- ####################### BLOCK 1 - Causes "internal error: delayed" ##########################

    -- FROM_NOW, FROM_NOW
    await_stable(sl, 50 ns, FROM_NOW, 100 ns, FROM_NOW, ERROR, "sl: Stable FROM_NOW, FROM_NOW, OK after 50 ns", C_SCOPE);

    sl <= transport not sl after 30 ns;
    await_stable(sl, 50 ns, FROM_NOW, 100 ns, FROM_NOW, ERROR, "sl: Stable FROM_NOW, FROM_NOW, OK after 80 ns", C_SCOPE);

    sl <= transport not sl after 30 ns;
    await_stable(sl, 50 ns, FROM_NOW, 60 ns, FROM_NOW, ERROR, "sl: Not stable FROM_NOW, FROM_NOW, Fail after 30 ns", C_SCOPE);
    increment_expected_alerts(ERROR, 1);

    await_stable(sl, 50 ns, FROM_NOW, 1 ns, FROM_NOW, ERROR, "sl: Timeout before stable_req, FROM_NOW, FROM_NOW, Fail immediately", C_SCOPE);
    increment_expected_alerts(ERROR, 1);

    await_stable(sl, 0 ns, FROM_NOW, 0 ns, FROM_NOW, ERROR, "sl: stable for 0 ns, FROM_NOW, FROM_NOW, OK after 0 ns", C_SCOPE);


    -- FROM_LAST_EVENT, FROM_NOW
    sl <= not sl;
    log("NOTE: ERROR HAPPENS AFTER HERE");
    wait for 10 ns; 
    log("NOTE: ERROR HAPPENS BEFORE HERE");
    
    -- ####################### END OF BLOCK 1 ##########################
    
    
    
        
    -- ####################### BLOCK 2 - Code works if this is removed ##########################
    
    -- Pulse a certain number of clock periods
    clk100M_ena <= true; -- Clock must be running
    sl <= '0';
    wait for 0 ns;  -- Wait for signal to update
    gen_pulse(sl, clk100M, 10, "Test pulse 10 clk periods");
    check_value(sl'delayed(0 ns)'last_event, 10*C_CLK100M_PERIOD, ERROR, "Check start of pulse");
    wait for 0 ns; -- Wait for signal to be updated
    check_value(sl, '0', ERROR, "pulse for 10 clk periods, pulse done", C_SCOPE);
    check_value(sl'last_event, 0 ns, ERROR, "pulse for 10 clk periods. Check that it actually pulsed for a delta cycle", C_SCOPE);
    check_value(sl'last_value, '1', ERROR, "pulse for 10 clk periods, check that it actually pulsed for a delta cycle", C_SCOPE);
    wait for 100 ns;
    
    -- ####################### END OF BLOCK 2 ##########################

    --==================================================================================================
    -- Ending the simulation
    --------------------------------------------------------------------------------------
    wait for 1000 ns;  -- to allow some time for completion
    report_alert_counters(INTERMEDIATE);
    report_alert_counters(FINAL);
    log(ID_LOG_HDR,"SIMULATION COMPLETED", C_SCOPE);
    assert false
      report "End of simulation.  (***Ignore this failure. Was provoked to stop the simulation.)"
      severity failure;
    wait;  -- to stop completely


  end process p_main;

end func;

