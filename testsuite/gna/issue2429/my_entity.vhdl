library IEEE;
use     IEEE.std_logic_1164.all;

package my_pkg is
	generic (
		ADDRESS_BITS  : positive
	);

	type T_AXI4LITE_BUS_M2S is record
		AWValid     : std_logic;
		AWAddr      : std_logic_vector;
	end record;
	type T_AXI4LITE_BUS_M2S_VECTOR is array(natural range <>) of T_AXI4LITE_BUS_M2S;


	subtype SIZED_M2S is T_AXI4LITE_BUS_M2S(
		AWAddr(ADDRESS_BITS - 1 downto 0)
	);

	subtype SIZED_M2S_VECTOR is T_AXI4LITE_BUS_M2S_VECTOR(open)(
		AWAddr(ADDRESS_BITS - 1 downto 0)
	);

end package;




library IEEE;
use     IEEE.std_logic_1164.all;

entity my_entity is

end entity;


architecture rtl of my_entity is


	package sized_record is
	new work.my_pkg
		generic map(
			ADDRESS_BITS  => 8
		);

	signal DeMux_M2S : sized_record.Sized_M2S_Vector(0 to 2);

begin

end architecture;
