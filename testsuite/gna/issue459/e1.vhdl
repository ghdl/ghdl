entity E1 is
end entity;

architecture behav of E1 is
	-- array with unconstrained array element type
	type    A is array(natural range <>) of bit_vector;
	
begin
end architecture;
