entity E is
end entity;

architecture A of E is
	-- array with unconstrained array element type
	type    A is array(natural range <>) of bit_vector;
	
	-- partially constrained array -> constrained inner array (element)
	subtype P1 is A(open)(7 downto 0);
	-- partially constrained array -> constrained outer array (vector)
	subtype P2 is A(15 downto 0)(open);
	
	signal S1 : P1(15 downto 0);         -- finally constraining the vector size
	signal S2 : P2(open)(7 downto 0);    -- finally constraining the element size          line 14
begin
end architecture;
