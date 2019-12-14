library ieee;
use     ieee.std_logic_1164.all;

entity test is
end entity;

architecture a of test is
	type RegisterNames is (
		LENGTH,
		PATTERN_0, PATTERN_1,
		COLOR_0, COLOR_1, COLOR_2, COLOR_3
	);
	type RegisterFile is array(RegisterNames) of std_logic_vector(31 downto 0);
	
	signal Reg : RegisterFile := (
		LENGTH => 32d"4",
		PATTERN_0 => (
			1 downto 0 => "01",  -- line 18:  can't match string literal with type anonymous enumeration subtype defined at std_logic_1164.v08:89:32
			3 downto 2 => "11",  -- line 19:  can't match string literal with type anonymous enumeration subtype defined at std_logic_1164.v08:89:32
			5 downto 4 => "01",  -- line 20:  can't match string literal with type anonymous enumeration subtype defined at std_logic_1164.v08:89:32
			7 downto 6 => "10",  -- line 21:  can't match string literal with type anonymous enumeration subtype defined at std_logic_1164.v08:89:32
			others => '0'
		),
		COLOR_0 => x"----_00_00",
		COLOR_1 => x"----_00_FF",
		COLOR_2 => x"----_FF_33",
		COLOR_3 => x"----_CC_99",
		others => (others => '0')
	);
begin
end architecture;
