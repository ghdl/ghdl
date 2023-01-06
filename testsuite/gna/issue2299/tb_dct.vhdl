library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb_dct8 is
end tb_dct8;

architecture beh of tb_dct8 is
    signal r : real;
begin
    process
        variable line0 : line;
    begin
        r <= 123.0;
        write(line0, to_string(r, 8));
        writeline(output, line0);
        wait;
    end process;
end architecture;
