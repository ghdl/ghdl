-- Author: Patrick Lehmann
--
-- VHDL-2008 subprogram instantiation (generic subprograms).
package Generics is
	function generic_add generic (type T) (x, y : T) return T;
	procedure generic_proc generic (type U) (x : U);
end package Generics;

package body Generics is
	function generic_add generic (type T) (x, y : T) return T is
	begin
		return x + y;
	end function;

	procedure generic_proc generic (type U) (x : U) is
	begin
	end procedure;
end package body Generics;

use work.Generics.all;

package Instances is
	function add_int is new work.Generics.generic_add generic map (T => integer);
	procedure proc_int is new work.Generics.generic_proc generic map (U => integer);
end package Instances;
