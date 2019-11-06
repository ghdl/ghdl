library ieee;
use ieee.std_logic_1164.all;
use std.env.stop;

entity test is
    generic (
        SIM : boolean := false
        );
    port (
        val : in std_ulogic
        );
end entity test;

architecture behaviour of test is
begin
    process_0: process(all)
    begin
        if SIM and val = '1' then
            stop;
        end if;
    end process;
end architecture behaviour;
