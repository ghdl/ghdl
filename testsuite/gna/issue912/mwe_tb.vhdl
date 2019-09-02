library ieee;
use ieee.std_logic_1164.all;

entity mwe_tb is
end mwe_tb;

architecture testbench of mwe_tb is
    component mwe_entity is
        port(
            input : in std_logic;
            output: out std_logic
        );
    end component;
    signal i,o : std_logic;
begin
    dut:mwe_entity
    port map(input => i,output => o);

    process
    begin
        for t in 0 to 10 loop
            i <= '1';
            wait for 5 ns;
            assert o = '1' severity failure;
            i <= '0';
            wait for 5 ns;
            assert o = '0' severity failure;
        end loop;
        wait;
    end process;
end testbench;
