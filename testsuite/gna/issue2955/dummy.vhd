library ieee;
use ieee.numeric_std.all;
use work.types_pkg.all;

-- Both port declarations below are from the report's "not OK" cases:
-- a fully unconstrained nested array type, and one with only the outer
-- dimension constrained (inner "signed" elements left unconstrained).
entity dummy is
	generic (
		g_bit_width : positive;
		g_num_chnl  : positive
	);
	port (
		i_data  : in  signed_vector;
		i_data2 : in  signed_vector(0 to g_num_chnl - 1);
		o_data  : out signed_vector(0 to g_num_chnl - 1)(g_bit_width - 1 downto 0)
	);
end entity;

architecture behavioral of dummy is
begin
	o_data <= i_data2;
end architecture;
