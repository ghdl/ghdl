library ieee;
use ieee.std_logic_1164.all;

entity child_1 is
	port (
		x : in std_logic;
		y : out std_logic
	);
end;

architecture a of child_1 is
begin
	y <= x;
end;

-----------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity ent_1 is
	port (
		a : in std_logic;
		b : in std_logic;
		q : out std_logic
	);
end;

architecture a of ent_1 is
	component child_comp is
		port (
			x : in std_logic;
			y : out std_logic
		);
	end component;
begin
	child_inst: child_comp
		port map (
			x => a,
			y => q
		);
end;

-----------------------------------

configuration conf_1 of ent_1 is
	for a
		for child_inst : child_comp
			use entity work.child_1;
		end for;
	end for;
end configuration;
