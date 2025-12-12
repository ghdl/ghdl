library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity T18_Timer is
    generic (ClockFrequency : integer);
    port (
        Clk    : in std_logic;
        nRst   : in std_logic;    --  Negative reset.
        Seconds : inout integer;
        Minutes : inout integer;
        Hours   : inout integer
    );
end entity;

architecture rtl of T18_Timer is
    --  Signal for counting clock periods.
    signal Ticks : integer;
begin
    process (Clk)
    begin
        if rising_edge (Clk) then
            --  If the negative reset is active.
            if nRst = '0' then
                Ticks   <= 0;
                Seconds <= 0;
                Minutes <= 0;
                Hours   <= 0;
            else
                if Ticks = ClockFrequency - 1 then
                    Ticks <= 0;

                    --  True every minute.
                    if Seconds = 59 then
                        Seconds <= 0;

                        --  True every hour.
                        if Minutes = 59 then
                            Minutes <= 0;

                            --  True every day.
                            if Hours = 23 then
                                Hours <= 0;
                            else
                                Hours <= Hours + 1;
                            end if;
                        else
                            Minutes <= Minutes + 1;
                        end if;
                    else
                        Seconds <= Seconds + 1;
                    end if;
                else
                    Ticks <= Ticks + 1;
                end if;
            end if;
        end if;
    end process;
end architecture;
