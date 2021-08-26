library IEEE;
use IEEE.std_logic_1164.all;

entity tb is
end entity;
architecture a of tb is
        signal clk : std_logic := '0';
        signal running : boolean := true;
begin
        clk <= not clk after 5 ns when running else '0';

        process(clk)
                -- This assignment one throws a proper error message:
                --constant var : integer := integer'high+1;
        begin
                if(rising_edge(clk)) then
                        -- Looping out of integer range results in Bug-Message

                        -- Over int'high
                        for i in integer'high to integer'high+1 loop
                        end loop;

                        -- Under int'low
                        for i in integer'low downto integer'low-1 loop
                        end loop;
                        running <= false;
                end if;
        end process;
end architecture;
