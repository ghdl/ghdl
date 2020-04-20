package pkg_B is
	generic (
		B: 	integer := 2
	);
	procedure showB;
end pkg_B;
package body pkg_B is
	procedure showB is 
	begin
		report "B:" & integer'image(B);
	end procedure showB;
end package body pkg_B;
