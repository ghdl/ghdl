library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port(
		pi   : in  std_logic;
		po   : out std_logic
	);
end ent;

architecture synth of ent is

	signal sig : std_logic := '0';

	attribute keep : string;
	attribute keep of sig : signal is "true";

begin

	sig <= pi;
	po <= sig;

end architecture;
