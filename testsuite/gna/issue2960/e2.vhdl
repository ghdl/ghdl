-- rm -f work-obj93.cf && ghdl -a e.vhd && ghdl -r e
library ieee;
use ieee.std_logic_1164.all;

entity e2 is
end entity;

architecture a of e2 is
	function myfn(dummy: natural)
		return integer
	is
		variable v_slv : std_ulogic_vector(3 downto 0) := "0000";
	begin
		v_slv(14 downto 0) := "000";
		return 42;
	end function;
	constant X : integer := myfn(0);
begin
end architecture;
