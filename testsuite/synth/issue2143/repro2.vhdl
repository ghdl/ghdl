entity repro2 is
  port (clk : bit;
        o : out bit);
end;

architecture struct of repro2 is
	type entry_t is record
		a : bit;
	end record;

	type table_t is array (natural range<>, natural range<>) of entry_t;

	function fun return table_t is
		variable ret : table_t(0 to 7, 0 to 7);
	begin
		return ret;
	end function;

        constant table : table_t := fun;
begin
  o <= table(0,0).a when clk = '1' else '0';
end architecture;
