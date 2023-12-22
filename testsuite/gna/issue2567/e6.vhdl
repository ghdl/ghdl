ENTITY e6 IS 
END;

ARCHITECTURE a OF e6 IS 
	TYPE r IS RECORD 
		s : bit_vector(2 DOWNTO 0);
                g : bit_vector(3 DOWNTO 0);
	END RECORD r;

	SIGNAL s : r;
BEGIN
	s <= (s|g => xxx);
END ARCHITECTURE a;
