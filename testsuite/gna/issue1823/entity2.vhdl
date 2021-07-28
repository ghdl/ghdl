entity entity2 is
end;

architecture behav of entity2 is
	signal Clock : bit;
begin
	inst2: counter2(a to b)
		port map (
			clk => Clock
		);
end architecture behav;
