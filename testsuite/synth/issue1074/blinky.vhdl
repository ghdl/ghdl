library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;

entity blinky is
port ( 
           clk : in  std_logic;
           led : out std_logic
    );
end blinky;

architecture rtl of blinky is
constant max_count : natural := 48000000;
signal rst : std_logic;
begin
    rst <= '0';
    -- 0 to max_count counter
    counter : process(clk, Rst)
    variable count : natural range 0 to max_count;
    begin
    if rising_edge(clk) then
        if count < max_count/2 then
            count := count + 1;
            led <= '1';
        elsif count < max_count then
            led <= '0';
            count := count + 1;
        else
            led <= '1';
            count := 0;
        end if;
    elsif rst = '1' then
        count := 0;
        led <= '1';
    end if;
    end process counter; 
end rtl;
