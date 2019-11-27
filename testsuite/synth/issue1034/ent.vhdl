use work.p.all;

entity ent is
	generic (
		WIDTH : natural := 8
	);
	port (
		test : in bit_vector(id(WIDTH)-1 downto 0)
	);
end ent;

architecture a of ent is
begin
end a;
