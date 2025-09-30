library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity ent is
	port (sig1 : in std_logic;
              sig2 : in std_logic;
              sig3 : out std_logic);

end entity;

architecture a of ent is
begin
	sig3 <= sig2 xor sig1;
end;
