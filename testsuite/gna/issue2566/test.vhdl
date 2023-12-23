library ieee;
use ieee.std_logic_1164.all;

entity child is
	port ( foo: out std_logic := '0' );
end entity;

architecture a of child is
begin
	process
	begin
          assert foo = '0';
		wait for 100 ns;
		foo <= not foo;
		wait for 100 ns;
		wait;
	end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity test is
end entity;

architecture a of test is
	signal foosig: std_logic;
begin
	-- `foo` is initialized to '0'
	child_assoc: entity work.child port map ( foo => foosig );

	-- `foo` is initialized to 'U'
	child_noassoc: entity work.child port map ( foo => open );
	-- same as above
	--child: entity work.child;
end architecture;
