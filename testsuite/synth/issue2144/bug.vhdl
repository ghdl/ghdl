library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
	port (
		dummy : in std_ulogic
	);
end bug;

architecture struct of bug is
	type table_t is array (natural range<>, natural range<>) of std_ulogic;

	function fun return table_t is
		variable ret : table_t(0 to 1, 0 to 3);
	begin
		return ret;
	end function;

	constant table : table_t := fun;
	
begin
	gen_i : for i in table'range(1) generate
		gen_j : for j in table'range(2) generate
			b : block is
				function print return std_ulogic is
				begin
					report "index="& integer'image(i) & "," & integer'image(j) & "; " &
					       "length="& integer'image(table'length(1)) & "," & integer'image(table'length(2));
					return '0';
				end function;

				constant tmp : std_ulogic := print;
				constant entry : std_ulogic := table(i, j);
			begin

			end block;
		end generate;
	end generate;
end architecture;
