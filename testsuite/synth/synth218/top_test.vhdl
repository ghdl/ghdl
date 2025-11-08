library ieee;
use ieee.std_logic_1164.all;

entity test is
	port (
		a : in std_logic;
		x : out std_logic
	     );
begin
end entity;

architecture a of test is
begin
	x <= a;
end architecture a;

architecture b of test is
begin
	x <= not a;
end architecture b;

library ieee;
use ieee.std_logic_1164.all;

entity top_test is
	port (
		a : in std_logic;
		x1 : out std_logic;
		x2 : out std_logic
	     );
begin
end entity;

architecture rtl of top_test is
begin
	inst_test_a: entity work.test(a)
	port map(
		a => a,
		x => x1
		);
	inst_test_b: entity work.test(b)
	port map(
		a => a,
		x => x2
		);
end architecture;
