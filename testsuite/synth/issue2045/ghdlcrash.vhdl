library ieee;
use ieee.std_logic_1164.all;

entity ghdlcrash is
	port (
		i : in  std_logic;
		o : out std_logic
	);
end ghdlcrash;

architecture synth of ghdlcrash is

	-- Utility function to calculate minimum of two values
	function min(a, b : natural) return natural is
		variable m : natural := 0;
	begin

		-- This line makes GHDL crash
		m := a when a <= b else b;

		-- This works
		if a <= b then m := a ; else m := b; end if;

		return m;
	end function;

	-- Initialize general input grouping
	constant CST : natural := min(0, 0);

begin

	-- Phony functionality
	o <= i;

end architecture;
