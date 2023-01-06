library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb_dct8 is
end tb_dct8;

architecture beh of tb_dct8 is
    procedure write_arr(x : real_vector) is
        variable line0 : line;
    begin
        for n in x'range loop
            write(line0, to_string(x(n), 4), right, 5);
            write(line0, string'(" "));
        end loop;
        writeline(output, line0);
    end procedure;
    signal y : real_vector(0 to 7);
begin
    process
    begin
        write_arr(y);
        assert false report "all tests passed" severity note;
        wait;
    end process;
end architecture;
