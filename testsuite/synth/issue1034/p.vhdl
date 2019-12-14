package p is
	function id(val : natural) return natural;
end package;

package body p is
	function id(val : natural) return natural is
	begin
		return val;
	end function;
end package body;

