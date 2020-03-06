entity ent is
end;

architecture arch of ent is

  type range_t is array (integer range <>) of bit;

  function get_range (constant code : string) return range_t is
    constant ret_val : range_t(1 to 10) := (others => '0');
  begin
    return ret_val;
  end function get_range;

  signal ret_val : boolean_vector(get_range("HELLO")'range) := (others => false);

  --constant ret_range : range_t := get_range("HELLO");
  --signal ret_val : boolean_vector(ret_range) := (others => false);
begin
end;
