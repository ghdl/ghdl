entity crash_entity is end entity;
architecture default of crash_entity is
        type rec is record
          v : natural;
        end record;
	attribute s : rec;
	function func return boolean is
	begin
		return s.v = 0;
	end function;
begin
end architecture;
