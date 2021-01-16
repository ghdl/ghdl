library ieee;
use ieee.std_logic_1164.all;
use work.v;

-- TOP BUG ---
entity ent_bug is end;
architecture RTL of ent_bug is
begin
	inst_v : entity v
	port map (input => "0000");
end;
