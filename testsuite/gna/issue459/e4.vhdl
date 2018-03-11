entity E4 is
end entity;

architecture A of E4 is
	-- array with unconstrained array element type
	type    A is array(natural range <>) of bit_vector;
	
	-- partially constrained array -> constrained inner array (element)
	subtype P1 is A(open)(7 downto 0);
	
	signal S1 : P1(15 downto 0);         -- finally constraining the vector size
begin
end architecture;
