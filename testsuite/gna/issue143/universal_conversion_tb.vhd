use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;

entity universal_conversion_tb is
end universal_conversion_tb;

architecture test of universal_conversion_tb is 

    constant TEST_VAL : time := 1000.0/100 * 1 ns;
 
begin

    process
	variable l : line;
    begin
	write(l, string'("TEST_VAL = "));
	write(l, TEST_VAL);
	writeline(output, l);
	wait;
    end process;

end;
