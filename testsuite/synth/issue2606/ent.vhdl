library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		clk : in std_logic;
		a : in std_logic;
		b : in std_logic;
		c : in std_logic;
		q : out std_logic
	);
end;

architecture b of ent is
begin
	process(clk)
	begin
		if rising_edge(clk) and a = '1' then
			if b = '1' then
				if c = '1' then
					-- nothing
				else
					q <= '1';
				end if;
			end if;
		end if;
	end process;
end;
