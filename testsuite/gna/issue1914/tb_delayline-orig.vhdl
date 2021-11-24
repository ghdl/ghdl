library IEEE;
use IEEE.std_logic_1164.all;

library vunit_lib;
context vunit_lib.vunit_context;

entity tb_delayline is
    generic (
       delay : natural := 10;
       runner_cfg : string
    );
end entity;

architecture rtl of tb_delayline is
    signal clk : std_logic := '0';
    signal i, o : std_logic_vector(7 downto 0);
begin

    -- Runner setup and timeout time
    test_runner_setup(runner, runner_cfg);
    test_runner_watchdog(runner, 10 ms);
    
    -- 100 Mhz clock
    clk <= not clk after 5 ns;

    -- Test Start
    process
    begin
        while test_suite loop     
            if run("test") then
                check_equal(1, 1, "yay");
            end if;
        end loop;
        test_runner_cleanup(runner);
        wait;
    end process;


    -- Device Under Test 
    c0: entity work.delayline(rtl)
        generic map (
            delay => delay
        )
        port map (
            clk => clk,
            i => i, 
            o => o
        );
end architecture;

