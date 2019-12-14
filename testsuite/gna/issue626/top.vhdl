library ieee;
use ieee.std_logic_1164.all;

entity inc_ent is
	generic (
		works : integer;
		vec : std_logic_vector);
end entity;

architecture default of inc_ent is
begin
	assert false report integer'image(works) & " " & integer'image(vec'length);
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity top_ent is end entity;

architecture default of top_ent is
	constant foo_v : std_logic_vector(0 to 12) := (others => '1');
begin
	g : for ix in 0 to 4 generate
		constant foo_v : std_logic_vector(0 to ix) := (others => '1');
	begin
		
		inst : entity work.inc_ent
			generic map (
				works => 0,
				vec   => (0 to ix => '1')
				);
		inst2 : entity work.inc_ent
			generic map (
				works => 1,
				vec => foo_v
				);
	end generate;
end architecture;
