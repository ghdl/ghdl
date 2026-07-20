-- Author: Patrick Lehmann
--
-- Functions and a procedure, exercising ReturnType, IsPure, and parameter capture.
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

	procedure log(msg : string; level : natural) is
	begin
	end procedure;
begin
end architecture rtl;
