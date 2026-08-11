-- Author: Patrick Lehmann
--
-- Functions and a procedure, exercising ReturnType, IsPure, parameter capture, and subprogram
-- body translation (declared items and statements).
entity Subprograms is
end entity Subprograms;

architecture rtl of Subprograms is
	function double(x : integer) return integer is
	begin
		return x * 2;
	end function;

	impure function get_random return integer is
	begin
		return 4;
	end function;

	function scale(x : integer) return integer is
		constant FACTOR : integer := 3;
		variable result : integer;
	begin
		return x * FACTOR;
	end function;

	procedure log(msg : string; level : natural) is
	begin
	end procedure;
begin
end architecture rtl;
