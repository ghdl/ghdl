-- Test case for VCD timestamp emission.
-- Three signals are updated at different intervals.
-- When filtering to only dump low_freq, the VCD should only contain
-- timestamps when low_freq actually changes, plus a final timestamp.

library IEEE;
use ieee.std_logic_1164.all;

entity tb is
end entity;

architecture behav of tb is
    constant semiperiod      : time := 5 ns;
    signal clk               : std_logic := '0';
    signal low_freq          : std_logic := '0';
    signal counter           : natural   := 0;
begin
    clk <= not clk after semiperiod;
    low_freq <= '1' when counter = 0 else '0';

    process (clk)
    begin
        if rising_edge(clk) then
            if counter = 9 then
                counter <= 0;
            else
                counter <= counter + 1;
            end if;
        end if;
    end process;
end;
