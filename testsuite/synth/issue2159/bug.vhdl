library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
	generic (
		LEN_DATA     : positive := 32;
		MAX_STEP     : positive := 8
	);
	port (
		src :  in std_ulogic_vector(LEN_DATA-1 downto 0);
		dst : out std_ulogic_vector(LEN_DATA-1 downto 0);
		
		step : in integer range 0 to MAX_STEP-1
	);
end bug;

architecture rtl of bug is
	constant STEP_SIZE : positive := 2;
begin
	dst <= src ror step*STEP_SIZE;
end architecture;
