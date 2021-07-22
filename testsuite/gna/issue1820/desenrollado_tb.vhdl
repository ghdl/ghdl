library IEEE;
use IEEE.std_logic_1164.all;

entity desenrollado_tb is
end entity ; -- desenrollado_tb

architecture desenrollado_tb_arq of desenrollado_tb is

	constant SIZE: natural := 16;

	signal vec_tb: std_logic;
	signal x_i_tb: std_logic_vector(SIZE-1 downto 0);
	signal y_i_tb: std_logic_vector(SIZE-1 downto 0);
	signal z_i_tb: std_logic_vector(SIZE-1 downto 0);
	signal x_o_tb: std_logic_vector(SIZE-1 downto 0);
	signal y_o_tb: std_logic_vector(SIZE-1 downto 0);
	signal z_o_tb: std_logic_vector(SIZE-1 downto 0);

begin

	vec_tb <= '1', '0' after 600 ns;
	x_i_tb <= (4 => '0', others => '1');
	y_i_tb <= (4 => '0', others => '1');
	z_i_tb <= (4 => '0', others => '1');

	cordic: entity work.cordic(desenrollado)
		port map(
			vec => vec_tb,
			x_i => x_i_tb,
			y_i => y_i_tb,
			z_i => z_i_tb,
			x_o => x_o_tb,
			y_o => y_o_tb,
			z_o => z_o_tb
		) ;
end architecture ; -- desenrollado_tb_arq
