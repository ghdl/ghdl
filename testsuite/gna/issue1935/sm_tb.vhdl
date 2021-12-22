library ieee;
use ieee.std_logic_1164.all;

entity SM_tb is 
end SM_tb;

	architecture test of SM_tb is		
		
		component clock is
			port(
				interval: in time;
				EN      : in std_logic;
				CLK     : out std_logic
			);
		end component;
		
		component bwc is
		port
		(
			enable, clock, reset : in std_logic;
			button_pressed       : in std_logic;
			open_window, close_window    : out std_logic;
		);
		end component;
		
		signal enable: std_logic := '0';		
		
		signal CLK0, CLK1, CLK2, CLK3: std_logic;
		signal CLKenable: std_logic := '0';
		
		signal o1, o2: std_logic;

		
	begin	
		clk_0 : clock port map (CLK => CLK0, interval => 0.5 ns, EN=> CLKenable);
		clk_1 : clock port map (CLK => CLK1, interval => 1 ns, EN=> CLKenable);
		clk_2 : clock port map (CLK => CLK2, interval => 2 ns, EN=> CLKenable);
		clk_3 : clock port map (CLK => CLK3, interval => 4 ns, EN=> CLKenable);
				
		sm_entity : bwc port map (enable => enable, clock => clk0, reset => '1', button_pressed => CLK1, open_window => o1, close_window => o2)
		-- enable <= '0';
		-- b <= '0';
		
		process begin
			wait for 1 ns;
			CLKenable <= '1';
			wait for 1 ns;
			enable <= '1';
			wait for 10 ns;

			-- b <= '1';
		
			-- a <= 'X';
			-- b <= 'X';
			-- wait for 10 ns;
			
			
			-- enable <= '0';
			-- b <= '0';
			-- wait for 10 ns;
			
			-- b <= '1';
			-- wait for 1 ns;
			


			assert false report "Test is done.";
			wait;
		
		end process;

	end architecture;