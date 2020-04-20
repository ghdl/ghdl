package pkg_A is
        --------------------------------------
	generic (
		A: 	integer := 1
	);
        --------------------------------------
	-- constant A: integer := 1;
        --------------------------------------
	package pkgB is new work.pkg_B
	generic map(
		B => A+1
	);
	procedure showA;
end pkg_A;
package body pkg_A is
	procedure showA is 
		use pkgB.showB;
	begin
		report "A:" & integer'image(A);
		showB;
	end procedure showA;
end package body pkg_A;
