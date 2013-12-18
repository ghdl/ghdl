entity e is
end entity e;
architecture test of e is
signal s : string(1 to 9)  := "2.345 Mhz";
signal s2 : string(1 to 9) := "2345 khz ";

	type frequency is range -2147483647 to 2147483647 units KHz;
		MHz = 1000 KHz;
		GHz = 1000 MHz;
	end units;

signal f : frequency := 3.456 MHz;

begin                               
	test : process is
	

	begin
		assert frequency'image(2 MHz) = "2000 khz";  
		assert frequency'image(f) = "3456 khz";  

                assert frequency'value("2000 khz") = 2 MHz ; 
		assert frequency'value("2345 khz") = 2.345 MHz ;
		assert frequency'value("2 MHz") = 2000 kHz ;

		assert frequency'value("2.345 Mhz") = 2345 kHz ;
                assert frequency'value(s) = 2345 kHz ; 
                assert frequency'value(s2) = 2345 kHz ;
		wait;
	end process;

end architecture test;
