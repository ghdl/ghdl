entity E2 is
end entity;

architecture behav of E2 is
	-- array with unconstrained array element type
	type    A is array(natural range <>) of bit_vector;

        signal s : a (7 downto 0)(3 downto 0);
begin
end architecture;
