ENTITY e2 IS 
END;

ARCHITECTURE a OF e2 IS 
	TYPE r IS RECORD 
		f : bit_vector(3 DOWNTO 0);
		s : bit_vector(2 DOWNTO 0);
	END RECORD r;

	SIGNAL s : r;
BEGIN
	s <= (s|g => "000");
END ARCHITECTURE a;
