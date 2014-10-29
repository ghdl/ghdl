library ieee;
use ieee.std_logic_1164.all;

entity works is
	generic (
		width : integer := 8
	);
	port(
		x : in std_logic;
		y : out std_logic_vector(width-1 downto 0);
		z : out std_logic
	);
end works;

architecture a of works is
	component subcomponent is
		generic (
			w : integer
		);
		port(
			x : in std_logic;
			y : out std_logic_vector(w-1 downto 0)
		);
	end component;
begin

	s : subcomponent
	generic map(
		w => width+1
	)
	port map(
		x => x,
		y(8 downto 1) => y,
		y(0) => z
	);

end a;


