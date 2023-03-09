

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
    signal a,b,c,d,e: std_logic;
begin

    COMBINATORIC: process( all ) is
    begin
        case a is
            when '0' =>
                with b select c <=
                    '0' when '1',
                    '1' when '0',
                    '0' when others;
            when others =>
                null;
        end case;
    end process;

end architecture rtl;
