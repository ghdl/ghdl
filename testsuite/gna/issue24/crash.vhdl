package shared_pkg is
	subtype shared_byte is integer range 0 to 255;
	type ram_t is protected
		procedure set(constant index : in integer; variable v : in shared_byte);
		impure function get(index : integer) return shared_byte;
	end protected ram_t;
end package shared_pkg;

package body shared_pkg is
	type ram_t is protected body
		type memory_array_t is array(natural range <>) of shared_byte;
		variable mem : memory_array_t(0 to 2**16-1);

		procedure set(constant index : in integer; variable v : in shared_byte) is
		begin
			mem(index) := v;
		end procedure set;

		impure function get(index : integer) return shared_byte is
		begin
			return mem(index);
		end function get;
	 end protected body ram_t;
end shared_pkg;

library IEEE;
use IEEE.std_logic_1164.all;
use work.shared_pkg.all;

entity ent1 is
end ent1;

architecture impl of ent1 is
	shared variable RAM : ram_t;
begin
end impl;

library IEEE;
use IEEE.std_logic_1164.all;
use work.shared_pkg.all;
use work.all;

entity test is
end test;

architecture func of test is
begin

	u1: entity work.ent1;

	process
		alias ram is << variable u1.RAM : ram_t >>;
		variable v : integer := 1;
	begin
		ram.set(0,v);
		wait;
	end process;

end func;
