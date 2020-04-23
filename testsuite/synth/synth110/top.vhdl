library ieee;
use ieee.std_logic_1164.all;

entity top is
    port(
        clk  : in  std_logic;
	di   : in  std_logic;
        do   : out std_logic
    ); 
end top;

architecture behavioral of top is
    signal data : std_logic;
begin

    mylabel: process (clk)
        variable tmp : std_logic;
    begin
        if rising_edge(clk) then
            tmp := di;              -- Post-synthesis name : mylabel.tmp
        end if;
        data <= not(tmp);           
    end process;

    do <= not(data);
    
end behavioral;
