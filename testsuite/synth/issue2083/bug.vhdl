library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
	port (
		clk : in std_ulogic
	);
end bug;

architecture behav of bug is
	type fields_t is record
		field_a : std_ulogic_vector;
		field_b : std_ulogic;
	end record;

	type field_array_t is array(natural range<>) of fields_t;
	
	function fun return std_ulogic is
		variable field_array : field_array_t(0 to 1)(field_a(0 to 31));
	begin		
		if field_array(0).field_b = '1' then -- this causes the crash
			--nothing
		end if;
		return '0';
	end function;
	
	constant data : std_ulogic := fun;
begin

end architecture;

