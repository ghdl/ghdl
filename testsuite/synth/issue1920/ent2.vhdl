library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		inputs : in  std_logic;
		result : out std_logic
	);
end entity;

architecture synth of ent is

	function local_func(A : std_logic) return std_logic is
	begin
		return A;
	end function;

	-- With this line
	-- As expected, GHDL emits a good enough error message: cannot use signal value during elaboration
	constant C : std_logic := inputs;

	-- But with this line, which is erroneous code, because a constant cannot be computed from a non-constant
	-- GHDL crashes with an assertion: raised CONSTRAINT_ERROR : netlists-builders.adb:791 access check failed
	--constant C : std_logic := local_func(inputs);

begin

	result <= C;

end architecture;
