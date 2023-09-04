library ieee;
use ieee.std_logic_1164.all;

entity foo is
end entity;

architecture fum of foo is
    type some_enum is (a, b, c, d, e, f, g);
    signal state:   some_enum;
begin
    process
    begin
        for i in some_enum'LEFT to some_enum'RIGHT loop
            state <= i;
            wait for 10 ns;
            report LF & HT & "state = " & some_enum'image(state);
        end loop;
        wait for 10 ns;
        wait;
    end process;
end architecture;
