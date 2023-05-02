package my_pkg1 is
	generic (ADDRESS_BITS  : positive);

	type T_AXI4LITE_BUS_M2S is record
		AWValid     : bit;
		AWAddr      : bit_vector;
	end record;
	type T_AXI4LITE_BUS_M2S_VECTOR is array(natural range <>) of T_AXI4LITE_BUS_M2S;

	subtype SIZED_M2S is T_AXI4LITE_BUS_M2S(
		AWAddr(ADDRESS_BITS - 1 downto 0)
	);

	subtype SIZED_M2S_VECTOR is T_AXI4LITE_BUS_M2S_VECTOR(open)(
		AWAddr(ADDRESS_BITS - 1 downto 0)
	);
end package;

entity repro1 is
end entity;

architecture rtl of repro1 is
	package sized_record is new work.my_pkg1
		generic map(ADDRESS_BITS  => 8);

	signal DeMux_M2S : sized_record.Sized_M2S_Vector(0 to 2);
begin
end architecture;
