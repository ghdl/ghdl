library ieee;
use ieee.std_logic_1164.all;

library std;
use std.textio.all;

entity test is
	port (
		clock : in std_logic
	);
end test;

architecture test_arc of test is
	impure function minified_func return boolean is
		file fref, fref2: text;
	begin
		return false;
	end function;
begin

end test_arc;

