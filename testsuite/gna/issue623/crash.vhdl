entity crash_entity is end entity;

architecture default of crash_entity is
	type foo_t is record
		a, b : bit;
	end record;

	function func return bit is
		variable v : foo_t(0 to 1);
	begin
		return '1';
	end function;
begin
end architecture;
