library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mwe is
end mwe;

architecture lulz of mwe is

constant cnt_len	: integer := 2;
constant cnt_stages	: integer := 2;

type ctl_sig is array (natural range <>) of std_logic_vector(cnt_len-1 downto 0);
signal ctl_cnt 	: ctl_sig(0 to cnt_stages-1);

signal clk	: std_logic := '0';

begin
	clk <= not clk after 50 ns;

	controller : entity work.counter
	generic map(
		width => cnt_len
	)
	port map(
		clk => clk,
		q => ctl_cnt(0)
	);

	ctl_cnt_delay : process
	begin
		wait until rising_edge(clk);
		for i in 0 to cnt_stages-2 loop
			-- uncomment following line to see that the port map assignment works
			--  and that this line just "overwrites" it..
			ctl_cnt(i+1) <= ctl_cnt(i);
		end loop;
	end process;

end lulz;
