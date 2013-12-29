use std.textio.all;
library ieee;
use ieee.numeric_bit.all;

entity bug is
end bug;

architecture misbehave of bug is
	begin
		process
    	variable l : line;
		begin
			write (l, bit'image('1'));
			writeline (output, l);
			wait;
		end process;
end misbehave;
