entity E is
end entity;

architecture A of E is
	signal S1           : bit := '0';
	signal S2_inertial  : bit;
	signal S2_transport : bit;
	signal S2_delayed   : bit;
	
	constant LEVEL : severity_level := FAILURE;
begin
	S1 <= '1' after 10 ns, '0' after 20 ns;
	
	S2_inertial  <= inertial S1 after 100 ns;
	S2_transport <= transport S1 after 100 ns;
	S2_delayed   <= S1'delayed(100 ns);
	
	CheckInertial: process
	begin
		wait until S2_inertial = '1' for 200 ns;
		assert (S2_inertial = '0') report "Pulse was not rejected!" severity LEVEL;
		wait;
	end process;

	CheckTransport: process
	begin
		wait until S2_transport = '1' for 115 ns;
		assert (S2_transport = '1') report "Pulse was not transport delayed!" severity LEVEL;
		assert (now = 110 ns)       report "Transport delayed pulse was not received at 110 ns!" severity LEVEL;
		wait;
	end process;
	
	CheckDelayed: process
	begin
		wait until S2_delayed = '1' for 115 ns;
		assert (S2_delayed = '1') report "Pulse was not delayed!" severity LEVEL;
		assert (now = 110 ns)     report "Delayed pulse was not received at 110 ns!" severity LEVEL;
		wait;
	end process;
end architecture;
