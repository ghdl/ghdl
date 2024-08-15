library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity T19_Timer is
generic(ClockFrequency : integer);
port(
    Clk     : in std_logic;
    nRst    : in std_logic; -- Negative reset
    Seconds : inout integer;
    Minutes : inout integer;
    Hours   : inout integer
);
end entity;

architecture rtl of T19_Timer is
    signal Ticks : integer;

    procedure Increment(signal Counter : inout integer) is
    begin
        Counter <= Counter + 1;
    end procedure;
begin
    process(Clk) is
    begin
        if rising_edge(Clk) then
            if nRst = '0' then
                Seconds <= 0;
                Minutes <= 0;
                Hours   <= 0;
                Ticks   <= 0;
            else
                if Ticks = ClockFrequency - 1 then
                    Ticks <= 0;

                    if Seconds = 59 then
                        Seconds <= 0;

                        if Minutes = 59 then
                            Minutes <= 0;

                            if Hours = 23 then
                                Hours <= 0;
                            else
                                Increment(Hours);
                            end if;
                        else
                            Increment(Minutes);
                        end if;
                    else
                        Increment(Seconds);
                    end if;
                else
                    Increment(Ticks);
                end if;
            end if;
        end if;
    end process;
end architecture;
