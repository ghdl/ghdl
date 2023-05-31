-----------------------------------------------------------------------------------------------------------------------------------------------------
-- 
--    design:              debouncer_tb
--
--    creation date:       01.03.23
--    VHDL standard:       2008
--
--    author:              Sebastian Schmitz
--    copyright:           Lumino Licht Elektronik GmbH
--
--
-----------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.textio.all;

library vunit_lib;
context vunit_lib.vunit_context;
context vunit_lib.vc_context;

library osvvm;
use osvvm.RandomPkg.all;


-- debouncer testbench
entity debouncer_not_ok_tb is
    generic(runner_cfg : string);
end entity;


architecture testbench of debouncer_not_ok_tb is

    component debouncer
        generic(
            DEBOUNCE_LENGTH : natural := 63);
        port(
            clk_i   : in  std_logic;
            reset_i : in  std_logic;
            key_i   : in  std_logic;
            key_o   : out std_logic);
    end component;

    shared variable RANDOM_GEN   : RandomPType;
    --
    constant CLK_PERIOD          : time      := 50 ns;                                              -- 20MHz
    constant DELTA_DELAY         : time      := 0.1 ns;
    constant WATCHDOG_TIMEOUT    : time      := 3 ms;
    --
    constant DEBOUNCE_LENGTH     : natural   := 31;
    constant RANDOM_TESTS_REPEAT : positive  := 300;
    --
    -- stimuli signals
    signal clk_i                 : std_logic := '1';
    signal reset_i               : std_logic;
    --
    signal key_i                 : std_logic;
    signal key_o                 : std_logic;


begin


    -- test environment 
    --------------------------------------------------------------------------------------------------------------------
    Testenvironment : block
    begin

        -- clock generate
        --------------------------------------------------------
        clock : clk_i <= not clk_i after (CLK_PERIOD / 2);


        -- watchdog
        --------------------------------------------------------
        watchdog : test_runner_watchdog(runner, WATCHDOG_TIMEOUT);


        -- device under test
        --------------------------------------------------------
        device_under_test : debouncer
            generic map(
                DEBOUNCE_LENGTH => DEBOUNCE_LENGTH)
            port map(
                clk_i   => clk_i,
                reset_i => reset_i,
                key_i   => key_i and '1',
                key_o   => key_o);

    end block;


    -- test_runner
    --------------------------------------------------------------------------------------------------------------------
    test_runner : process
        variable stable_time : integer;
    begin
        
        -- Put test suite setup code here
        test_runner_setup(runner, runner_cfg);
        --disable_stop(error);
        --disable_stop(failure);

        while test_suite loop

            -- Put common test case setup code here
            wait for CLK_PERIOD;
            reset_i <= '1';
            wait for CLK_PERIOD;
            reset_i <= '0';
            wait for DELTA_DELAY;

            -- reset
            -----------------------------------------------------
            if run("reset") then
                info("testing reset conditions");
                reset_i <= '1';
                wait for DELTA_DELAY;
                check(key_o = '0', result("for key_o."));


            -- key_debounce_push
            -----------------------------------------------------
            elsif run("key_debounce_push") then
                info("testing key debouncing when push the key");

                for i in 0 to RANDOM_TESTS_REPEAT - 1 loop
                    -- testing with random delay
                    key_i       <= '1';
                    stable_time := RANDOM_GEN.RandInt(1, 3 * DEBOUNCE_LENGTH);
                    info("testing with random generated stable time :0x" & to_integer_string(std_logic_vector(to_unsigned(stable_time, 16))));
                    for j in 0 to stable_time loop
                        wait for CLK_PERIOD;
                    end loop;
                    if stable_time > DEBOUNCE_LENGTH then
                        check(key_o = '1', result("for key_o = '1'."));
                    else
                        check(key_o = '0', result("for key_o = '0'."));
                    end if;

                    -- reset the key
                    key_i <= '0';
                    wait for (DEBOUNCE_LENGTH + 1) * CLK_PERIOD;

                end loop;


            -- key_debounce_release
            -----------------------------------------------------
            elsif run("key_debounce_release") then
                info("testing key debouncing when release the key");

                for i in 0 to RANDOM_TESTS_REPEAT - 1 loop

                    -- set the key
                    key_i <= '1';
                    wait for (DEBOUNCE_LENGTH + 1) * CLK_PERIOD;

                    -- testing with random delay
                    key_i       <= '0';
                    stable_time := RANDOM_GEN.RandInt(1, 3 * DEBOUNCE_LENGTH);
                    info("testing with random generated stable time :0x" & to_integer_string(std_logic_vector(to_unsigned(stable_time, 16))));
                    for j in 0 to stable_time loop
                        wait for CLK_PERIOD;
                    end loop;
                    if stable_time > DEBOUNCE_LENGTH then
                        check(key_o = '0', result("for key_o = '0'."));
                    else
                        check(key_o = '1', result("for key_o = '1'."));
                    end if;

                end loop;
                
                
            end if;

            -- Put common test case cleanup code here

        end loop;

        -- Put test suite cleanup code here
        test_runner_cleanup(runner);

    end process;



end architecture;
