-- Author: Patrick Lehmann
--
-- Subprogram declarations without a body (forward declarations, e.g. a package spec whose body
-- lives in a separate package body design unit) were previously silently dropped entirely.
package SubprogramsWithoutBodies is
	function foo(x : integer) return integer;
	procedure bar(x : integer);
end package SubprogramsWithoutBodies;

package body SubprogramsWithoutBodies is
	function foo(x : integer) return integer is
	begin
		return x + 1;
	end function;

	procedure bar(x : integer) is
	begin
	end procedure;
end package body SubprogramsWithoutBodies;
