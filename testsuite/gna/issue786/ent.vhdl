entity ent is
end ent;

architecture a of ent is
	signal sig_x : bit_vector(2 downto 0);
	signal sig_y : bit_vector(2 downto 0);

	signal sig_z : bit_vector(3 downto 0);
begin
	-- works
--	(sig_x(1), sig_x(0), sig_y(1), sig_y(0)) <= sig_z;

	-- "raised CONSTRAINT_ERROR : trans-chap3.adb:3058 access check failed"
	(sig_x(1 downto 0), sig_y(1 downto 0)) <= sig_z;
end a;
