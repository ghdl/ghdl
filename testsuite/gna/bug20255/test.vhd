entity e is
end entity e;
architecture test of e is
begin                               
	test : process is
	
	type frequency is range -2147483647 to 2147483647 units KHz;
		MHz = 1000 KHz;
		GHz = 1000 MHz;
	end units;
	begin
		assert frequency'image(2 MHz) =  "2000 khz";  -- this should work, but GHDL produces an error
		wait;
	end process;

end architecture test;
