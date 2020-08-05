entity repro is
	generic (
		BUS_WIDTH : integer := 8);
end entity;

architecture Behav of repro is
	type BusT is record
		A : bit_vector;
		F : bit_vector;
	end record;

	signal BusInst : BusT(
		A(BUS_WIDTH-1 downto 0),
		F(3 downto 0)
	);
begin

end architecture;
