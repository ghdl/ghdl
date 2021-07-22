library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.math_real.all;

entity cordic is
	generic (
		SIZE: natural := 16
	);
	port (
		vec: in std_logic;
		x_i: in std_logic_vector(SIZE-1 downto 0);
		y_i: in std_logic_vector(SIZE-1 downto 0);
		z_i: in std_logic_vector(SIZE-1 downto 0);
		x_o: out std_logic_vector(SIZE-1 downto 0);
		y_o: out std_logic_vector(SIZE-1 downto 0);
		z_o: out std_logic_vector(SIZE-1 downto 0)
	) ;
end entity ; -- cordic

architecture desenrollado of cordic is

	type matriz is array(natural range <>) of std_logic_vector(SIZE-1 downto 0);
	signal x, y, z: matriz(0 to SIZE);
	signal a: std_logic := '1';

	--type matriz_atan is array (0 to SIZE-1) of std_logic_vector(SIZE-1 downto 0);
	--constant atan: matriz_atan := (
	--	for i in 0 to SIZE-1 loop
	--		std_logic_vector(),
	--	end loop ; 
	--)
	signal PRUEBA: std_logic_vector(SIZE-1 downto 0) := std_logic_vector(to_unsigned(integer(0.25*2**SIZE+0.5), SIZE));
	constant HOLA: real := 0.5;

begin

	x(0) <= x_i;
	y(0) <= y_i;
	z(0) <= z_i;

	x_o <= x(SIZE);
	y_o <= y(SIZE);
	z_o <= z(SIZE);

	etapas : for i in 0 to SIZE-1 generate
		etapa: entity work.etapa
			generic map(
				SIZE => SIZE,
				STEP => i
			)
			port map(
				vec => vec,
				x_i => x(i),
				y_i => y(i),
				z_i => z(i),
				atan_i => (others => '0'),
				x_o => x(i+1),
				y_o => y(i+1),
				z_o => z(i+1)
			);
	end generate ; -- etapas

end architecture ; -- desenrollado
