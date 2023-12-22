ENTITY e3 IS 
END;

ARCHITECTURE a OF e3 IS 
	TYPE r IS RECORD 
		f : bit_vector(3 DOWNTO 0);
		s : bit_vector(2 DOWNTO 0);
	END RECORD r;

	SIGNAL s : r;
BEGIN
	s <= (others => "000");
END ARCHITECTURE a;
