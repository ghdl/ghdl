library IEEE;
use IEEE.std_logic_1164.all;

entity a is
	port(
		irq : out std_ulogic
	);
end a;

library IEEE;
use IEEE.std_logic_1164.all;

entity b is
	generic(
		NUM_CHANNELS : positive := 4
	);
	port(
		src_channel :  out integer range 0 to NUM_CHANNELS-1;
		src_valid :  in std_ulogic;
		src_ready : out std_ulogic
	);
end b;

architecture struct of a is

	signal src_channel     : integer range 0 to 0;
	signal src_valid       : std_ulogic;
	signal src_ready       : std_ulogic;
begin
	u0 : entity work.b
	generic map(
		NUM_CHANNELS => 1
	)
	port map(
		src_channel => src_channel,
		src_valid   => src_valid,
		src_ready   => src_ready
	);
end architecture;

architecture behav of b is
begin
	process(all)
		variable ready         : std_ulogic;
		variable channel_ready : std_ulogic;
	begin
		ready := '1';
		for i in 0 to NUM_CHANNELS-1 loop
			if i = src_channel and src_valid = '1' then
				channel_ready := '0';
			else
				channel_ready := '1';
			end if;
			ready := ready and channel_ready;
		end loop;

		src_ready <= ready;
		src_channel <= NUM_CHANNELS-1;
	end process;

end architecture;
