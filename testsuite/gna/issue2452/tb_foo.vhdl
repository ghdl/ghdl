---------------------------------------------------------------------
---------------------------------------------------------------------
---                                                               ---
---             ____ _   _ ____  ____                             ---
---            / ___| \ | |  _ \/ ___|                            ---
---           | |   |  \| | |_) \___ \                            ---
---           | |___| |\  |  _ < ___) |                           ---
---            \____|_| \_|_| \_\____/                            ---
---            ____ _____    ___ _   _ ____  _   _                ---
---           |  _ \_   _|  |_ _| \ | / ___|| | | |               ---
---           | | | || |_____| ||  \| \___ \| | | |               ---
---           | |_| || |_____| || |\  |___) | |_| |               ---
---           |____/ |_|    |___|_| \_|____/ \___/                ---
---                                                               ---
---                                                               ---
---------------------------------------------------------------------
---------------------------------------------------------------------
library std; 
	use std.env.stop;	

library ieee;
    use ieee.std_logic_1164.all;    

entity tb_foo is 
	generic 
    (  g_rst_polar : std_ulogic := '1'
	 ; g_sync_rst  : boolean    := false
    );
end tb_foo;
architecture tb of tb_foo is  

	signal s_clk_i : std_ulogic := '0';
	signal s_rst_i : std_ulogic := '1';
	signal s_bit_i : std_ulogic;
	signal s_bit_o : std_ulogic;

begin 
	s_clk_i <= not(s_clk_i) after 1 ns;
	s_rst_i <= '1' after 10 ns;

	P_Scheduler:
	process
	is
	begin
		s_bit_i <= '0';
		wait for 100 ns;

		stop;
		wait;
	end process;
	
	E_Uut:
	entity work.foo(behavioral)
	generic map
		( g_rst_polar => g_rst_polar
		, g_sync_rst  => g_sync_rst )
    port map
		( i_clk => s_clk_i
		, i_rst => s_rst_i
		, i_bit => s_bit_i
		, o_bit => s_bit_o );
end tb;