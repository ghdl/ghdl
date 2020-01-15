library ieee;
use ieee.std_logic_1164.all;

entity xor_gate is
	generic (
		INVERT : boolean
	);
	port (
		a : in std_logic;
		b : in std_logic;
		q : out std_logic
	);
end;

architecture a of xor_gate is
begin
	gen: if INVERT generate
		q <= not (a xor b);
	else generate
		q <= a xor b;
	end generate;
end;
library ieee;
use ieee.std_logic_1164.all;

entity top is
	port (
		x : in std_logic;
		y : in std_logic;
		o_custom : out std_logic;
		o_and : out std_logic
	);
end;

architecture a of top is
	component comp is
		port (
			a : in std_logic;
			b : in std_logic;
			q : out std_logic
		);
	end component;
begin
	comp_inst: comp
		port map (
			a => x,
			b => y,
			q => o_custom
		);

	o_and <= x and y;
end;
configuration conf of top is
	for a
		for comp_inst : comp
			use entity work.xor_gate
				generic map (
					INVERT => false
				);
		end for;
	end for;
end configuration;
