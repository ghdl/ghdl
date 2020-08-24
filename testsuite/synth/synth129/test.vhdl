library ieee;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;


entity test is
	port(
		sel: 			in std_logic;
		inp:			in unsigned(7 downto 0);
		outp:			out integer range 0 to 255
		);
end;

architecture a of test is

begin
	with sel select outp <= to_integer(inp) when '1', 0 when others;		
end;	

