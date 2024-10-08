library ieee;
use ieee.std_logic_1164.all;

entity innertest2 is
	port (
		d : in std_logic;
		q : out std_logic
	);
end innertest2;

architecture beh of innertest2 is
begin
  q <= d;
end beh;

library ieee;
use ieee.std_logic_1164.all;

entity test2 is
	port (
		d_i : in std_logic;
		q_o : out std_logic
	);
end test2;

architecture beh of test2 is begin
	itinst : entity work.innertest2(beh)
	port map ( d_i, q_o);
end beh;

