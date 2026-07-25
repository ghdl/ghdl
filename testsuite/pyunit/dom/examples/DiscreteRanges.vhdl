-- Author: Patrick Lehmann
--
-- Discrete ranges in all their forms, including the subtype-indication ones.
entity DiscreteRanges is
end entity DiscreteRanges;

architecture rtl of DiscreteRanges is
	signal vector : bit_vector(0 to 7);
begin
	-- A discrete range may be a subtype indication, not just a range expression.
	gen: for i in integer range 0 to 3 generate
	end generate;

	process is
		variable v : bit_vector(0 to 7);
	begin
		-- Constrained subtype indication as the loop's discrete range.
		for i in integer range 0 to 3 loop
			null;
		end loop;

		-- Bare type mark as the loop's discrete range.
		for i in bit loop
			null;
		end loop;

		-- Range attribute name (already supported, guarded against regression).
		for i in v'range loop
			null;
		end loop;

		-- Plain range expression (already supported, guarded against regression).
		for i in 0 to 3 loop
			null;
		end loop;

		wait;
	end process;
end architecture rtl;
