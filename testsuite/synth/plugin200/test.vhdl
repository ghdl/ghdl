library ieee;
use ieee.std_logic_1164.all;

entity innertest is
	port (
		input : in std_logic;
		output : out std_logic
	);
end innertest;

architecture beh of innertest is
begin
  output <= input;
end beh;

library ieee;
use ieee.std_logic_1164.all;

entity test is
	port (
		input : in std_logic;
		output : out std_logic
	);
end test;

architecture beh of test is begin
	itinst : entity work.innertest(beh)
	port map ( input => input, output => output );
end beh;

