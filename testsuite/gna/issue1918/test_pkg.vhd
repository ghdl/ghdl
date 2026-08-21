package test_pkg is

	type value_t is record
		value : natural;
	end record;

	type value_array_t is array(natural range<>) of value_t;

	type state_t is record
		values : value_array_t;
	end record;

	type state_array_t is array(natural range<>) of state_t;

end test_pkg;

package body test_pkg is

	function to_string(prefix : string; val : state_array_t) return string is
	begin
		if val'length = 0 then
			return "";
		else
			return to_string(prefix, val(val'low+1 to val'high));
		end if;
	end function;

end package body test_pkg;
