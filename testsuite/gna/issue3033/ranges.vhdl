entity Ranges is
	port (
		signal Clock : in  bit;
		signal LED   : out bit
	);
end entity;


architecture test of Ranges is
	type int64 is range -9223372036854775807 - 1 to 9223372036854775807;

	type freq is range natural'low to natural'high units
		Hz;
		kHz = 1000 Hz;
		MHz = 1000 kHz;
		GHz = 1000 MHz;
		THz = 1000 GHz;
	end units;
	
	function contains(str : string; find : character) return boolean is
	begin
		for i in str'range loop
			return true when str(i) = find;
		end loop;
		return false;
	end function;
	
	constant freq_5Hz      : string := freq'image(5 Hz);
	constant freq_5Hz_unit : string := freq_5Hz(freq_5Hz'high - 1 to freq_5Hz'high);
begin
	-- IMPORTANT: activate assertions in synthesis settings
	
	assert integer'low   <= -9223372036854775807 -1 report "FAILED: integer'low   <= -9223372036854775808   ; integer'low   = " & integer'image(integer'low)    ; -- & " / " & to_image(integer'low);
	assert integer'high  >=  9223372036854775807    report "FAILED: integer'high  >=  9223372036854775807   ; integer'high  = " & integer'image(integer'high)   ; -- & " / " & to_image(integer'high);
	assert natural'low    =           0             report "FAILED: natural'low    =                    0   ; natural'low   = " & natural'image(natural'low)    ; -- & " / " & to_image(natural'low);
	assert natural'high  >=  9223372036854775807    report "FAILED: natural'high  >=  9223372036854775807   ; natural'high  = " & natural'image(natural'high)   ; -- & " / " & to_image(natural'high);
	assert positive'low   =           1             report "FAILED: positive'low   =                    1   ; positive'low  = " & positive'image(positive'low)  ; -- & " / " & to_image(positive'low);
	assert positive'high >=  9223372036854775807    report "FAILED: positive'high >=  9223372036854775807   ; positive'high = " & positive'image(positive'high) ; -- & " / " & to_image(positive'high);

	assert int64'low     <= -9223372036854775807-1  report "FAILED: int64'low     <= -9223372036854775808   ; int64'low     = " & int64'image(int64'low)    ; -- & " / " & to_image(int64'low);
	assert int64'high    >=  9223372036854775807    report "FAILED: int64'high    >=  9223372036854775807   ; int64'high    = " & int64'image(int64'high)   ; -- & " / " & to_image(int64'high);
											  
	assert time'low      <= -9223372036854775807 fs report "FAILED: time'low      <= -9223372036854775807 fs; time'low      = " & time'image(time'low)  ; -- & " / " & to_image(time'low);
	assert time'high     >=  9223372036854775807 fs report "FAILED: time'high     >=  9223372036854775807 fs; time'high     = " & time'image(time'high) ; -- & " / " & to_image(time'high);

	assert freq'low       =                    0 Hz report "FAILED: freq'low      <=                    0 Hz; freq'low      = " & freq'image(freq'low)  ; -- & " / " & to_image(freq'low);
	assert freq'high      =  9223372036854775807 Hz report "FAILED: freq'high     >=  9223372036854775807 Hz; freq'high     = " & freq'image(freq'high) ; -- & " / " & to_image(freq'high);
	
	assert freq_5Hz_unit = "hz"                     report "FAILED: Unit of '5 Hz' is 'Hz'; Unit = " & freq_5Hz_unit;
	assert not contains(freq_5Hz, '.')              report "FAILED: Physical type is an integer number";
	
	LED <= not LED when rising_edge(Clock);
end architecture;
