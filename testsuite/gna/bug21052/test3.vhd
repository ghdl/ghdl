package some_package is
	-- this signal seems to be problematic
	-- uncomment to reproduce the bug
        signal some_signal :bit;
	component some_component end component;
end package;

entity some_component is end entity;
architecture a of some_component is begin end architecture;

entity wrapper is end entity;
architecture a of wrapper is begin
	inst :work.some_package.some_component;
end architecture;

entity test is end entity;
architecture a of test is
	component wrapper is end component;
	signal dummy :bit;
begin
	inst :if false generate inst :wrapper; end generate;
	process begin
		wait for 1 ns; dummy <= '1';
		wait for 1 ns; dummy <= '0';
		wait;
	end process;
end architecture;
