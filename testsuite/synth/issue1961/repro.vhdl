entity repro is
	port (
		clk : in bit
	);
end;

architecture struct of repro is

	type a_t is record
		value : bit_vector;
	end record;

	type a_array_t is array(natural range<>) of a_t;

	type b_t is record
		a : a_array_t;
	end record;

	type b_array_t is array(natural range<>) of b_t;

	function fun return natural is
		variable b : b_array_t(0 to 1)(a(0 to 31)(value(31 downto 0)));
	begin
		return 0;
	end function;

	constant dummy : natural := fun;
begin
	
end architecture;
