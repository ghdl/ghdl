entity crash_entity is end entity;
architecture default of crash_entity is
	attribute s : string;
	function func return boolean is
	begin
		return s(1);
	end function;
begin
end architecture;
