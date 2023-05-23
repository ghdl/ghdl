entity ent is
end entity;

architecture a of ent is
	attribute TotalBits : integer;

	type ArrayType is array(natural range <>) of bit;
	attribute TotalBits of ArrayType : type is -1 * 1;

	type ArraySubtype is array(natural range 0 to 3) of bit;
	attribute TotalBits of ArraySubtype : subtype is ArraySubtype'length * 1;


	type MatrixType is array(natural range <>) of bit_vector;
	attribute TotalBits of MatrixType : type is -1;

	type MatrixSubType_1 is array(natural range <>) of bit_vector(7 downto 0);
	attribute TotalBits of MatrixSubType_1 : subtype is -1 * MatrixSubType_1'element'length;

	type MatrixSubType_2 is array(natural range 0 to 3) of bit_vector(7 downto 0);
	attribute TotalBits of MatrixSubType_2 : subtype is MatrixSubType_2'length * MatrixSubType_2'element'length;
begin
	assert false report to_string(ArrayType'TotalBits)    severity note;

	assert false report to_string(ArraySubtype'TotalBits) severity note;

	assert false report to_string(MatrixType'TotalBits) severity note;

	assert false report to_string(MatrixSubType_1'element'length) severity note;
	assert false report to_string(MatrixSubType_1'TotalBits) severity note;

	assert false report to_string(MatrixSubType_2'length) severity note;
	assert false report to_string(MatrixSubType_2'element'length) severity note;
	assert false report to_string(MatrixSubType_2'TotalBits) severity note;
end architecture;
