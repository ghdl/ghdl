entity test is end;
architecture test of test is
	signal a, b: boolean;
begin
	process (all) is begin
		with a select b <= true when false, false when true;
	end process;
end;
