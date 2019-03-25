entity ent is
end ent;

architecture a of ent is
	signal sig_x : bit;
	signal sig_y : bit;

	signal sig_z : bit_vector(1 downto 0);
begin
	-- works
	-- (sig_x, sig_y) <= sig_z;

	-- "raised TYPES.INTERNAL_ERROR : trans.adb:619"
	(sig_x, sig_y) <= not sig_z;
end a;
