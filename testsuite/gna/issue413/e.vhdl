library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity e is
end entity;

architecture arch of e is
	type t_record is record
		int : integer;
	end record;

	function conversion(slv : std_ulogic_vector) return t_record is
		variable ret : t_record;
	begin
		ret.int := to_integer(unsigned(slv));
		return ret;
	end;

	signal slv : std_ulogic_vector(127 downto 0);
	alias alias_record : t_record is conversion(slv);

	signal s_int : integer := alias_record.int;
begin
end architecture;
