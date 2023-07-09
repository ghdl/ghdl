library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
	port(
		a_in: in std_logic_vector(7 downto 0);
		b_out: out std_logic_vector(7 downto 0)
	);
end;

architecture formal of repro1 is
	function test_function(a: std_logic_vector) return std_logic_vector is
	begin
		assert a(0) = '1';
		return a;
	end;
begin
	b_out <= test_function(a_in);
end;
