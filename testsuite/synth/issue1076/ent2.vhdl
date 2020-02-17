library ieee;
use ieee.std_logic_1164.all;

entity ent2 is
	-- This case works.
	-- generic ( CONFIG_C1: boolean := false );
	port (
		i   : in std_logic;
		o   : out std_logic;
		q   : out std_logic
	);
        constant CONFIG_C1 : boolean := false;
end;

architecture a of ent2 is
   component c1 is
	port (i: in std_logic; o   : out std_logic);
   end component;

   component c2 is
	port (i: in std_logic; o   : out std_logic);
   end component;
begin
	gen: if false generate
		o <= '1';
	else generate
		o <= '0';
	end generate;

maybe_c1:
	if CONFIG_C1 generate
	c1_inst: c1 port map (i => i, o=> q);
	end generate;

maybe_c2:
	if not CONFIG_C1 generate
	c2_inst: c2 port map (i => i, o=> q);
	end generate;
		

end;

-- Added entities to satisfy simulation:

library ieee;
use ieee.std_logic_1164.all;

entity c1 is
	port (i: in std_logic; o   : out std_logic);
end entity;

architecture a of c1 is
begin
	o <= i;
end a;
	
library ieee;
use ieee.std_logic_1164.all;

entity c2 is
	port (i: in std_logic; o   : out std_logic);
end entity;

architecture a of c2 is
begin
	o <= i;
end a;

