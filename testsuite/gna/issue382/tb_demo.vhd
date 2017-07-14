library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity tb_demo is
end entity;

architecture v1 of tb_demo is
        signal clk,reset: std_logic;

        component demo is
               port (
                clk,reset: in std_logic;
                load:   in std_logic;
                load_val: in unsigned(7 downto 0);
                qout: out unsigned(7 downto 0);
                is5: out std_logic
                );                         
       end component;

        signal load, is5: std_logic;
        signal load_val,qout: unsigned(7 downto 0);
begin

UUT1: demo port map (clk, reset, load, load_val, qout, is5);

        process
        begin
                load<='0';
                for i in 0 to 3 loop
                        wait until clk='1';
                end loop;

                load<='1';
                load_val<=x"42";
                wait until clk='1';
        end process;

        process
        begin
                reset<='1';
                clk<='0';
                wait for 2 sec;
                reset<='0';
                while 1=1 loop
                        clk<='0';
                        wait for 0.5 sec;
                        clk<='1';
                        wait for 0.5 sec;
                end loop;
        end process;


end v1;
