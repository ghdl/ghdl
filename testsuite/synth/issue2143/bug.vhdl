library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
	port (
		dummy : in std_ulogic
	);
end bug;

architecture struct of bug is
	type entry_t is record
		a : std_ulogic;
	end record;

	type table_t is array (natural range<>, natural range<>) of entry_t;

	function fun return table_t is
		variable ret : table_t(0 to 7, 0 to 7);
	begin
		return ret;
	end function;

	constant table : table_t := fun;
	constant entry : entry_t := table(0, 0);
begin

end architecture;
