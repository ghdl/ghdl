library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity etapa is
	generic (
		SIZE: natural := 16;
		STEP: natural := 1
	);
	port (
		vec: in std_logic;
		x_i: in std_logic_vector(SIZE-1 downto 0);
		y_i: in std_logic_vector(SIZE-1 downto 0);
		z_i: in std_logic_vector(SIZE-1 downto 0);
		atan_i: in std_logic_vector(SIZE-1 downto 0);
		x_o: out std_logic_vector(SIZE-1 downto 0);
		y_o: out std_logic_vector(SIZE-1 downto 0);
		z_o: out std_logic_vector(SIZE-1 downto 0)
	) ;
end entity ; -- etapa

architecture etapa_arq of etapa is

	signal d_i: std_logic;
	signal d_i_slv: std_logic_vector(SIZE-1 downto 0);
	signal d_i_n_slv: std_logic_vector(SIZE-1 downto 0);

	signal x_shifted: std_logic_vector(SIZE-1 downto 0);
	signal y_shifted: std_logic_vector(SIZE-1 downto 0);
	signal c_x_shifted: std_logic_vector(SIZE-1 downto 0);
	signal c_y_shifted: std_logic_vector(SIZE-1 downto 0);
	signal c_z_i: std_logic_vector(SIZE-1 downto 0);

begin

	d_i <= y_i(SIZE-1) when vec = '1' else z_i(SIZE-1);
	d_i_slv <= (SIZE-2 downto 0 => '0') & d_i;
	d_i_n_slv <= (SIZE-2 downto 0 => '0') & not(d_i);

	x_shifted <= std_logic_vector(shift_right(signed(x_i), STEP));
	y_shifted <= std_logic_vector(shift_right(signed(y_i), STEP));

	c_x_shifted <= x_shifted xor (0 to SIZE-1 => d_i);
	c_y_shifted <= y_shifted xor (0 to SIZE-1 => not(d_i));
	c_z_i <= atan_i xor (0 to SIZE-1 => not(d_i));

	x_o <= std_logic_vector(signed(x_i) + signed(c_y_shifted) + signed(d_i_n_slv));
	y_o <= std_logic_vector(signed(y_i) + signed(c_x_shifted) + signed(d_i_slv));
	z_o <= std_logic_vector(signed(z_i) + signed(c_z_i) + signed(d_i_n_slv));

end architecture ; -- etapa_arq
