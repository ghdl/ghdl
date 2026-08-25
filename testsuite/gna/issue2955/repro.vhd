library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     ieee.math_real.all;

package dummy_pkg is
	type signed_vector is array(natural range <>) of signed;
end package;

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     ieee.math_real.all;
library work;
use     work.dummy_pkg.all;

entity dummy is
	generic
	(
		g_bit_width : positive;
		g_num_chnl  : positive
	);
	port
	(
		i_data : in  signed_vector(0 to g_num_chnl - 1);
		o_data : out signed_vector(0 to g_num_chnl - 1)(g_bit_width - 1 downto 0)
	);
end entity;

architecture rtl of dummy is
begin
	o_data <= i_data;
end architecture;

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     ieee.math_real.all;
library work;
use     work.dummy_pkg.all;

entity aaa_test_tle is
end entity;

architecture rtl of aaa_test_tle is
	constant c_bit_width : positive := 16;
	constant c_num_chnl  : positive := 1;
	signal i_data        : signed(c_bit_width - 1 downto 0) := (others => '0');
	signal o_data        : signed(c_bit_width - 1 downto 0);
begin
	i_dut : entity work.dummy
	generic map (g_bit_width => c_bit_width, g_num_chnl  => c_num_chnl)
	port map
	(
		i_data(0)   => i_data,
		o_data(0)   => o_data
	);
end architecture;
