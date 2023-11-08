library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bfm2 is
end entity;

architecture beh of bfm2 is
    signal data :unsigned(7 downto 0);
    signal en   :std_logic;
begin

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test2 is
end entity;

architecture beh of test2 is


begin

    ibfm: entity work.bfm2;
	
    -- The testbench process
    process
    alias probe_data is <<signal .test2.ibfm.data  :unsigned(7 downto 0)>>;
    alias probe_en   is <<signal .test2.ibfm.en    :std_logic>>;
    begin
        probe_en   <= '1';
        probe_data <= (others => '1');

        wait for 100 ns;
        probe_en   <= '0';
        probe_data <= (others => '0');
        
        wait;
    end process;
	
end architecture;
