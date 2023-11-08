library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bfm is
end entity;

architecture beh of bfm is
    signal data :unsigned(7 downto 0);
    signal en   :std_logic;
begin

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test1 is
end entity;

architecture beh of test1 is

    alias probe_data is <<signal .test1.ibfm.data  :unsigned(7 downto 0)>>;
    alias probe_en   is <<signal .test1.ibfm.en    :std_logic>>;

begin

    ibfm: entity work.bfm;
	
    -- The testbench process
    process
    begin
        probe_en   <= '1';
        probe_data <= (others => '1');

        wait for 100 ns;
        probe_en   <= '0';
        probe_data <= (others => '0');
        
        wait;
    end process;
	
end architecture;
