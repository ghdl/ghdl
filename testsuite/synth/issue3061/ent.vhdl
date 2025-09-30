library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity ent is
	port (
		sig1 : in std_logic;
		sig2 : out std_logic;
		sig3 : in std_logic
	);

end entity;

architecture a of ent is
begin
	sig2 <= sig1 xor sig3;
end;
