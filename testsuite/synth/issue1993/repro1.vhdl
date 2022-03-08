entity bug is
	port (
		clk :  in bit
	);
end bug;

architecture struct of bug is

  type a_t is record
    value : bit_vector;
  end record;

  type a_array_t is array(natural range<>) of a_t;

  type b_t is record
    a : a_array_t;
  end record;

  type b_array_t is array(natural range<>) of b_t;

  type table_t is array (natural range<>, natural range<>) of a_t;

  function make_b return b_array_t is
    variable ret : b_array_t(0 to 0)(a(0 to 31)(value(31 downto 0)));
  begin
    return ret;
  end function;

  function calculate_b return b_array_t
  is
    variable empty : b_array_t(0 to -1)(a(0 to 31)(value(31 downto 0)));
  begin
    return empty & make_b;
  end function;

  function calculate_table(b : b_array_t) return table_t is
    variable ret : table_t(0 to b'length-1, 0 to b(0).a'length-1)(value(31 downto 0));
  begin
    return ret;
  end function;

  constant b     : b_array_t := calculate_b;
  constant table : table_t := calculate_table(b);
begin

end architecture;

