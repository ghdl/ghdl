library ieee;
use ieee.std_logic_1164.all;

entity child is
	port (
		x : in std_logic;
		y : out std_logic
	);
end;

architecture a of child is
begin
	y <= x;
end;

-----------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		a : in std_logic;
		b : in std_logic;
		q : out std_logic
	);
end;

architecture a of ent is
	component child_comp is
		port (
			x : in std_logic;
			y : out std_logic
		);
	end component;
begin
	child_inst: child_comp
		port map (
			x => a and b,
			y => q
		);
end;

-----------------------------------

configuration conf of ent is
	for a
		for child_inst : child_comp
			use entity work.child;
		end for;
	end for;
end configuration;
