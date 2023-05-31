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


-- debouncer testbench
entity debouncer_no_vunit_tb is
end entity;


architecture testbench of debouncer_no_vunit_tb is

    component debouncer
        generic(
            DEBOUNCE_LENGTH : natural := 63);
        port(
            clk_i   : in  std_logic;
            reset_i : in  std_logic;
            key_i   : in  std_logic;
            key_o   : out std_logic);
    end component;

    --
    constant CLK_PERIOD          : time      := 5 ns;                                              -- 20MHz
    constant DELTA_DELAY         : time      := 0.1 ns;
    --
    constant DEBOUNCE_LENGTH     : natural   := 10;
    constant TESTS_REPEAT        : positive  := 3;
    --
    -- stimuli signals
    signal clk_i                 : std_logic := '1';
    signal reset_i               : std_logic;
    signal clk_enable               : std_logic := '1';

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
        clock : clk_i <= not clk_i after (CLK_PERIOD / 2) when clk_enable else '0';

    end block;

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



    -- test_runner
    --------------------------------------------------------------------------------------------------------------------
    test_runner : process
    begin


            -- Put common test case setup code here
            clk_enable <= '1';


            wait for CLK_PERIOD;
            reset_i <= '1';
            wait for CLK_PERIOD;
            reset_i <= '0';
            wait for DELTA_DELAY;

            -- key_debounce_push
            -----------------------------------------------------

            for i in 0 to TESTS_REPEAT - 1 loop
                -- testing with random delay
                key_i <= '1';
                for j in 0 to 30 loop
                    wait for CLK_PERIOD;
                end loop;

                -- reset the key
                key_i <= '0';
                wait for (DEBOUNCE_LENGTH + 1) * CLK_PERIOD;

            end loop;
            clk_enable <= '0';
            wait for 5*CLK_PERIOD;

    end process;



end architecture;
