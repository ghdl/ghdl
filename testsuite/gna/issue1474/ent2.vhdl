library ieee;
use ieee.std_logic_1164.all;

package pkg is

	type vector_array_t is array(natural range <>) of std_logic_vector;
	
	function concatenate(arr : vector_array_t) return std_logic_vector;
end package;

package body pkg is

    function concatenate(arr : vector_array_t) return std_logic_vector is
		constant ARR_SIZE : natural := arr'length;
		constant VEC_SIZE : natural := arr(arr'low)'length;
		variable ret : std_logic_vector(ARR_SIZE * VEC_SIZE - 1 downto 0);
	begin
		for r in arr'range loop
			ret((r+1) * VEC_SIZE - 1 downto r * VEC_SIZE) := arr(r);
		end loop;
		return ret;
	end function;

end package body;
library ieee;
use ieee.std_logic_1164.all;

library work;
use work.pkg.all;

entity ent2 is
end entity;
    
architecture a of ent2 is
	signal test : vector_array_t(7 downto 0)(7 downto 0);
	signal test2 : std_logic_vector(63 downto 0);

begin
	test2 <= concatenate(test);
end;
