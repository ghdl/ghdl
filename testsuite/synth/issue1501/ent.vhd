library ieee;
use ieee.std_logic_1164.all;

entity bug01 is
	port (
		signal r : in std_logic;
		signal s : in std_logic;
		signal q : out std_logic
	);
end entity;

architecture rtl of bug01 is
	signal res : std_logic;
begin
	process(r,s)
	begin
		if s = '1' then
			res <= '1';
		elsif r = '1' then
			res <= '0';
		end if;
	end process;

	q <= res;
end;
