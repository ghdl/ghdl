entity clock_Generator is
	port (
		Clock : out bit
	);
end entity;

entity clock_Monitor is
	port (
		Clock : in bit
	);
end entity;

package clock is
end package;


library Clock;
use     Clock.clock.all;

entity test is
end entity;

architecture tb of test is
  signal Clock        : bit;
begin
	gen: entity Clock.clock_Generator
		port map (
			Clock       => Clock
		);
	
	mon: entity Clock.clock_Monitor
		port map (
			Clock       => Clock
		);
end architecture;
