--  Analyse: ghdl -a t18_Timer.vhdl
--  Analyse: ghdl -a t18_TimerTb.vhdl
--  Run    : ghdl -r t18_TimerTb --vcd=./t18_TimerTb.vcd --stop-time=10sec/600sec/3660sec
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity T18_TimerTb is
end entity;

architecture sim of T18_TimerTb is
    --  Turn down resolution of the clock to speed up simulation.
    constant ClockFrequency : integer := 10; --  10 Hz.
    constant ClockPeriod    : time    := 1000 ms / ClockFrequency;

    signal Clk     : std_logic := '1';
    signal nRst    : std_logic := '0';
    signal Seconds : integer   := 0;
    signal Minutes : integer   := 0;
    signal Hours   : integer   := 0;

begin
    --  The Device Under Test (DUT).
    i_Timer : entity work.T18_Timer (rtl)
        generic map (ClockFrequency => ClockFrequency)
        port map (
            Clk     => Clk,
            nRst    => nRst,
            Seconds => Seconds,
            Minutes => Minutes,
            Hours   => Hours
        );

    --  Process for generating the clock.
    Clk <= not Clk after ClockPeriod / 2;

    --  Testbench sequence.
    process is
    begin
        wait until rising_edge (Clk);
        wait until rising_edge (Clk);

        --  Take the DUT out of reset.
        nRst <= '1';


        wait;
    end process;
end architecture;
