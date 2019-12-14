entity ent is
end ent;

architecture a of ent is
	signal s : string(1 to 3);
begin
	s <= "abc";

	process(all)
	begin
		case s is
			when "abc" =>
			when others =>
		end case;
	end process;
end a;
