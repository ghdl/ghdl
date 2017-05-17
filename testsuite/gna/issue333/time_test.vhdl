entity Time_test is
end Time_test;

architecture Behavioral of Time_test is

constant c_CLOCK_FREQUENCY : natural := 32000000;
constant c_SWITCH_ON_TIME_ms : time := 200 ms; 
constant c_SWITCH_COUNT_CYCLES : integer := integer(real(c_CLOCK_FREQUENCY  * 
                                    (c_SWITCH_ON_TIME_ms / 1 ms)) / 1000.0);

begin
process is
begin
        report " Switch ON time " & time'image(c_SWITCH_ON_TIME_ms) severity NOTE;        
        report " Switch count " & natural'image(c_SWITCH_COUNT_CYCLES) severity NOTE;
        -- sanity checks on time constraints
        report "Clock frequency =  " & natural'image(c_CLOCK_FREQUENCY) & " Hz" severity NOTE;
        report "Clock period =  " & time'image( 1 sec / c_CLOCK_FREQUENCY) severity NOTE;
        report "Switch period = " & time'image ( 1 sec / c_CLOCK_FREQUENCY * c_SWITCH_COUNT_CYCLES) severity NOTE;
        Assert c_SWITCH_ON_TIME_ms =  1 sec / c_CLOCK_FREQUENCY * c_SWITCH_COUNT_CYCLES 
            report "Wrong Switch ON time = " & time'image ( 1 sec / c_CLOCK_FREQUENCY * c_SWITCH_COUNT_CYCLES) severity ERROR;
        wait;
end process;
end Behavioral;
