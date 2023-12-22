ENTITY e IS 
END ENTITY e;
ARCHITECTURE a OF e IS 
	TYPE r IS RECORD 
		f : bit_vector(3 DOWNTO 0);
		s : bit_vector(2 DOWNTO 0);
	END RECORD r;

	SIGNAL s : r;
BEGIN
	s <= (s|f => "000");
END ARCHITECTURE a;
