package TypeDocumentation is
	--! An enumerated type.
	type stateA is (idle, busy);

	--! A second enumerated type.
	type stateB is (high, low);

	--! A record type.
	type frame is record
		a : bit;
	end record;

	--! An array type.
	type memory is array (0 to 3) of bit;

	--! An access type.
	type pointer is access integer;

	--! A file type.
	type storage is file of character;
end package;
