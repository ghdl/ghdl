entity InterfaceDocumentation is
	generic (
		--! Generic before.
		G1 : natural := 1;
		G2 : natural := 2;  --! Generic same line.

		--! Generic list.
		G3, G4 : natural := 3
	);
	port (
		--! Port before.
		Clock : in  bit;
		Reset : in  bit;  --! Port same line.
		                  --! Port continuation.

		--! Port list.
		A, B  : in  bit;
		Q     : out bit   --! Last port, no semicolon.
	);
end entity;
