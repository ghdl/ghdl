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
signal ctl_cnt_tmp 	: ctl_sig(0 to cnt_stages-1);

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

	-- workaround: use concurrent assignment of temporary signal
	bla : for k in 1 to cnt_stages-1 generate
		ctl_cnt(k) <= ctl_cnt_tmp(k);
	end generate bla;

	ctl_cnt_delay : process
	begin
		wait until rising_edge(clk);
		for i in 0 to cnt_stages-2 loop
			-- then this works...
			ctl_cnt_tmp(i+1) <= ctl_cnt(i);
		end loop;
	end process;

end lulz;
