-- Author: Patrick Lehmann
--
-- Aggregates using choice lists and the various choice kinds.
entity AggregateChoices is
end entity AggregateChoices;

architecture rtl of AggregateChoices is
	type rec is record
		a, b, c : bit;
	end record;
begin
	process is
		variable v : bit_vector(0 to 7);
		variable r : rec;
	begin
		-- Choice list: 'b' and 'c' share one associated expression.
		r := (a => '1', b | c => '0');

		-- Indexed, ranged and others choices side by side.
		v := (0 => '1', 1 to 3 => '0', others => '1');

		wait;
	end process;
end architecture rtl;
