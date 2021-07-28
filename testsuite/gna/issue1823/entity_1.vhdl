entity entity_1 is
end entity entity_1;

architecture behav of entity_1 is
	signal Clock : bit;
begin
	inst1: entity work.counter1(rtl)
		generic map (
			BITS => 8
		)
		port map (
			clk => Clock
		);

	inst2: counter2(rtl)
		port map (
			clk => Clock
		);

	inst3: component counter3
		port map (
			clk => Clock
		);

	inst4: configuration counter4
		port map (
			clk => Clock
		);

	blk: block
	begin
		inst5: entity work.counter1(rtl)
			port map (
				clk => Clock
			);
	end block;
end architecture behav;
