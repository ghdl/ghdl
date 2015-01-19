use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.vital_primitives.all;
use ieee.vital_timing.all;

entity numeric_tb is
    generic (
	tperiod_CLK_posedge : VitalDelayType := 0.000 ns);
    port (
	CLK : in std_ulogic);
    attribute VITAL_LEVEL0 of numeric_tb : entity is true;
end numeric_tb;

architecture test of numeric_tb is 
 
begin

    process
	variable l : line;
    begin
	write(l, string'("tperiod_CLK_posedge = "));
	write(l, tperiod_CLK_posedge);
	writeline(output, l);
	wait;
    end process;

end;
