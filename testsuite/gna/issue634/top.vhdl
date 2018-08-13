use std.textio.all;

entity top_ent is
end entity;

architecture default of top_ent is
	file fh : text;
begin
	process
	begin
		if endfile(fh) then
			null;
		end if;
	end process;
end architecture;
