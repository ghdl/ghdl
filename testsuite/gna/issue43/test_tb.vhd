library ieee;
use ieee.std_logic_1164.all;

library vunit_lib;
context vunit_lib.vunit_context;
context vunit_lib.com_context;

entity test_tb is
generic (runner_cfg : runner_cfg_t);

end entity;

architecture beh of test_tb is
  signal rx_data : std_logic_vector(159 downto 0);
  
  function to_b(constant s : string) return std_logic is
  begin
	return '0';
  end function;
  
  function to_a(constant s : string) return std_logic is
    variable a : std_logic := to_b(s);
  begin
	return '0';
  end function;
  
  procedure to_t( signal sa : out std_logic_vector(31 downto 0))  is
  begin
    sa <= (others => '1');
	assert false report "lol";
  end procedure;
begin
	
  asd : for i in 0 to 4 generate
  begin
	process
		constant s : string := "lane" & integer'image(i);
		variable self : actor_t := create(s);
	begin
		--assert false report "Error: " & s;
		
		rx_data(32*(i+1)-1 downto 32*i) <= (others => '0');
		wait for 10 ns;
		to_t(rx_data(32*(i+1)-1 downto 32*i));
		wait;
	end process;
  end generate;
  
  process
  begin
	test_runner_setup(runner, runner_cfg);
    while test_suite loop
		if run("test1") then
		null;
		end if;
	end loop;
    test_runner_cleanup(runner); -- Simulation ends here
  end process;

end architecture;