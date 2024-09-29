library ieee;
use ieee.std_logic_1164.all;
library unisim;
use unisim.vcomponents.all;

entity demo_comp is
		port (
		i, ib: in std_logic;
		o: out std_logic

);
end entity;

architecture demo of demo_comp is

begin

	c: ibufds
	generic map (
	    diff_term =>  true,
	    iostandard => "LVDS25"
    )
	port map (
		i => i,
		ib => ib,
		o => o
	);
end architecture;

