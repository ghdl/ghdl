library ieee;
use ieee.std_logic_1164.all;
 
entity scrambler_tb is
end entity scrambler_tb;
 
architecture behaviour of scrambler_tb is

    constant clk_period : time := 10 ns;
    
    signal clk, reset : std_logic;
    signal en, seed : std_logic;
    signal d_in, d_out : std_logic;

begin 

    uut: entity work.scrambler
	port map (
	    clk => clk,
	    reset => reset,
	    en => en,
	    seed => seed,
	    d_in => d_out,
	    d_out => d_out);
		  
   -- Clock process definitions
    clk_process: process
    begin
      for i in 1 to 10 loop
	clk <= '0';
	wait for clk_period/2;
	clk <= '1';
	wait for clk_period/2;
      end loop;
      wait;
    end process;

    -- Stimulus process
    stim_proc: process begin		

	-- Reset period
	reset <= '1';
	wait for clk_period * 2;	
	reset <= '0';
	wait for clk_period * 3.5;

	wait;
   end process;

end architecture behaviour;
