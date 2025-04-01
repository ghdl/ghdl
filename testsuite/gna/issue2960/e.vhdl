library ieee;
use ieee.std_logic_1164.all;

entity e is
end entity;

architecture a of e is
	function myfn(dummy: natural)
		return integer
	is
		variable v_undefined : integer;
		variable v_extreme : integer;
		variable v_mask_slv : std_ulogic_vector(3 downto 0) := "0000";
	begin
		v_extreme := 1 - v_undefined;
		v_mask_slv(-v_extreme downto 0) := "000";
		return 42;
	end function;
	constant X : integer := myfn(0);
begin
end architecture;
