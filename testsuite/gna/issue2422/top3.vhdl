library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity comp3 is
	port (
		output : out unsigned
	);
end entity;

architecture a1 of comp3 is
begin
	output <= (7 downto 0 => '0');
end architecture;

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity top3 is
end entity;

architecture a2 of top3 is
	function convert(val : unsigned) return std_logic_vector is
	begin
		return std_logic_vector(val);
	end function;

	signal sig : std_logic_vector(7 downto 0);
begin
	inst : entity work.comp3
		port map (
			convert(output) => sig
		);
end architecture;
