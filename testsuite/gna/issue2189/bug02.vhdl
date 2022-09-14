library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.all;

entity bug02 is
end bug02;

architecture rtl of bug02 is
    FUNCTION weird (bw : integer range 2 to 32)
        RETURN INTEGER IS
    BEGIN
        RETURN -(2**(bw - 1));
    END weird;
begin
    process
    begin
        report to_string(weird(10));
        wait;
    end process;
end architecture;
