library IEEE;
use IEEE.std_logic_1164.all;

entity bug2 is
	port (
		dummy : in std_ulogic
	);
end bug2;

architecture struct of bug2 is
	type table_t is array (natural range<>, natural range<>) of std_ulogic;

	function fun return table_t is
		variable ret : table_t(0 to 7, 0 to 7);
	begin
		return ret;
	end function;

	constant table : table_t := fun;
	constant entry : std_ulogic := table(0, 0);
begin

end architecture;
