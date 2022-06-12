library ieee;
use ieee.std_logic_1164.all;

entity identity is
	port (
		x: in std_logic_vector(7 downto 0);
		y: out std_logic_vector(7 downto 0)
	);
end entity;

architecture a of identity is
begin
	y <= x;
end architecture;

---

library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		a: in std_logic_vector(7 downto 0);
		b: out std_logic_vector(7 downto 0)
	);
end entity;

architecture a of ent is
	function transform(val: std_logic_vector) return std_logic_vector is
	begin
		return (7 downto 0 => '0');
	end function;
begin
	identity: entity work.identity port map (x => transform(a), y => b);
end architecture;
