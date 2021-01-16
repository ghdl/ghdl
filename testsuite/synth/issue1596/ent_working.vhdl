library ieee;
use ieee.std_logic_1164.all;
use work.v;

-- TOP WORKING ---
entity ent_working is end;
architecture RTL of ent_working is
	signal a : std_logic_vector(3 downto 0) := "0101";
begin
	inst_v : entity v
	port map (input => a);
end;
