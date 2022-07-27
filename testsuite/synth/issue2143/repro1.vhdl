entity repro1 is
end repro1;

architecture struct of repro1 is
	type entry_t is record
		a : bit;
	end record;

	type table_t is array (natural range<>, natural range<>) of entry_t;

	function fun return table_t is
		variable ret : table_t(0 to 7, 0 to 7);
	begin
		return ret;
	end function;

	constant entry : entry_t := fun(0, 0);
begin

end architecture;
