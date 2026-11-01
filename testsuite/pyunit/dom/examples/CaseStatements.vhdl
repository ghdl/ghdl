-- Author: Patrick Lehmann
--
-- Sequential case statements: an alternative immediately followed by 'others', and statements in 'when others'.
entity CaseStatements is
end entity CaseStatements;

architecture rtl of CaseStatements is
begin
	process is
		variable sel : integer := 0;
		variable a, b, c, d, e, f : integer := 0;
	begin
		case sel is
			when 0 =>
				a := 1;
			when 1 | 2 =>
				b := 2;
				c := 3;
			when others =>
				d := 4;
				e := 5;
				f := 6;
		end case;

		case sel is
			when 0 =>
				a := 1;
			when others =>
				b := 2;
				c := 3;
		end case;

		case sel is
			when 0 =>
				a := 1;
			when others =>
				null;
		end case;
		wait;
	end process;
end architecture rtl;
