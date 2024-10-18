library ieee;
use ieee.fixed_pkg.all;
use ieee.std_logic_1164.all;

entity inner is
	port ( some_port : in u_sfixed(3 downto -8) );
end entity;

architecture arch of inner is
begin
end architecture;

library ieee;
use ieee.fixed_pkg.all;
use ieee.std_logic_1164.all;

entity outer is
end entity;

architecture arch of outer is
component inner is
	port ( some_port : in u_sfixed(3 downto -8) );
end component;
	signal some_signal : u_sfixed(3 downto -8);
begin
	foo: inner
		port map (
			some_port => resize(some_signal, 3, -8)
		);
end architecture;
