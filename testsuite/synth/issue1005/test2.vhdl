library ieee;
use ieee.std_logic_1164.all;
use std.env.stop;

entity test2 is
    generic (
        SIM : boolean := false
        );
    port (
        val : in std_ulogic
        );
end entity test2;

architecture behaviour of test2 is
begin
    process_0: process(all)
    begin
        if not SIM or val = '1' then
            null;
        else
            stop;
        end if;
    end process;
end architecture behaviour;
