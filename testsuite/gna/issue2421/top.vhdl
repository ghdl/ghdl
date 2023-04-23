library ieee;
use     ieee.std_logic_1164.all;

entity comp is
	port (
		output : out std_logic_vector
	);
end entity;

architecture a1 of comp is
begin
	output <= (others => '0');
	-- output <= (output'range => '0');  -- gives no error
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
			output => sig
		);
end architecture;
