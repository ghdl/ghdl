library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity comp is
	port (
		output : out unsigned
	);
end entity;

architecture a1 of comp is
begin
	output <= (7 downto 0 => '0');  -- not using others due to issue #2421
end architecture;




library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity top is
end entity;

architecture a2 of top is
	signal sig : std_logic_vector(7 downto 0);
begin
	inst : entity work.comp
		port map (
			std_logic_vector(output) => sig
		);
end architecture;
