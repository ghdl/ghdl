library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity demo is
        port (
                clk,reset: in std_logic;
                load:   in std_logic;
                load_val: in unsigned(7 downto 0);
                qout: out unsigned(7 downto 0);
                is5: out std_logic
                );
end entity;

architecture v1 of demo is
        signal q: unsigned(7 downto 0); 
begin
        qout<=q;

--        is5<='1' when q=x"05" else '0';
        
        process(clk, reset)
        begin
                if reset='1' then
                        q<=(others=>'0');
                        is5<='0';
                elsif rising_edge(clk) then
                        is5<='0';
                        
                        if q=x"04" then
                                is5<='1';
                        end if;

                        if load='1' then
                                q<=load_val;
                                if load_val=x"05" then
                                        is5<='1';
                                end if;
                        else
                                q<=q+1;
                        end if;
                end if;
        end process;
end v1;
