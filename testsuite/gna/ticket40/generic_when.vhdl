library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity GENERIC_WHEN is
        generic( FOO  :     std_logic_vector(1 downto 0) );
        port(    IN1  : in  std_logic_vector(1 downto 0);
                 OUT1 : out std_logic_vector(1 downto 0) );
end GENERIC_WHEN;

architecture BEHAVIOUR of GENERIC_WHEN is
begin
        PR1:
        process( IN1 )
                variable l : line;
        begin
                case IN1 is
                        when FOO    => OUT1 <= FOO; write(l, string'("FOO")); writeline(output, l);
                        --when "00"   => write(l, string'("00")); writeline(output, l);
                        when "01"   => write(l, string'("01")); writeline(output, l);
                        when others => write(l, string'("other")); writeline(output, l);
                end case;
        end process PR1;
end BEHAVIOUR;

