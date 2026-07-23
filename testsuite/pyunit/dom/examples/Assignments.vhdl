-- Author: Patrick Lehmann
--
-- Conditional/selected/force/release assignment statements: previously the only gap remaining in
-- both the concurrent and sequential statement dispatchers (confirmed by a full audit before
-- starting this work).
entity Assignments is
end entity Assignments;

architecture rtl of Assignments is
	signal s, s2 : bit;
	signal sel : integer;
begin
	-- concurrent conditional signal assignment
	s <= '1' when sel = 0 else '0' when sel = 1 else 'Z';

	-- concurrent selected signal assignment, including a grouped choice ('0 | 1')
	with sel select
		s2 <= '1' when 0 | 1, '0' when 2, 'Z' when others;

	process is
		variable v, v2 : bit;
	begin
		-- sequential simple variable assignment
		v := '1';

		-- sequential conditional variable assignment (VHDL-2008)
		v2 := '1' when sel = 0 else '0' when sel = 1 else 'Z';

		-- sequential conditional signal assignment (VHDL-2008)
		s <= '1' when sel = 0 else '0';

		-- sequential selected variable assignment (VHDL-2008)
		with sel select
			v := '1' when 0, '0' when others;

		-- sequential selected signal assignment
		with sel select
			s <= '1' when 0, '0' when others;

		-- force/release (VHDL-2008)
		s <= force '1';
		s <= release;
	end process;
end architecture rtl;
