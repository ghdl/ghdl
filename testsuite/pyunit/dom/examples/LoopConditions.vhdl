-- Author: Patrick Lehmann
--
-- Loop statements with conditions on the loop itself and on exit/next statements.
entity LoopConditions is
end entity LoopConditions;

architecture rtl of LoopConditions is
begin
	process is
		variable i : integer := 0;
	begin
		while i < 10 loop
			exit when i = 5;
			next when i = 2;
			i := i + 1;
		end loop;
		wait;
	end process;
end architecture rtl;
