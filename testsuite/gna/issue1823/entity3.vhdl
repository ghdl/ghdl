entity entity3 is
end;

architecture behav of entity3 is
	signal Clock : bit;
begin
	inst2: counter2.all
		port map (
			clk => Clock
		);
end architecture behav;
