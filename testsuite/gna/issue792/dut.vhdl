entity dut is
	port(
		a : in string(1 to 1)
	);
end entity dut;
architecture a of dut is
	component dut_internal
	port(
		a : in string;
		b : in string
	);
	end component dut_internal;
	signal b : string(1 to 1);
begin
	inst : dut_internal
		port map (
		a => a
		-- b => b
	);
end architecture a;

