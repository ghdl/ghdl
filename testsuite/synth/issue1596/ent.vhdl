library ieee;
use ieee.std_logic_1164.all;

-- Instatiated component ---
entity v is
	-- input is unconstrained
	port (input : in std_logic_vector);
end;

architecture RTL of v is
	constant bits : positive := 4;
	signal i : std_logic_vector(bits - 1 downto 0);
begin
	i <= input;
end;
