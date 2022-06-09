library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
	port (
		src : in std_ulogic_vector(31 downto 0)
	);
end bug;

architecture rtl of bug is
	type array_t is array(0 to 0) of src'subtype;
	signal s : array_t;
begin

end architecture;
