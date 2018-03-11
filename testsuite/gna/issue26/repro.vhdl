entity Time_travel is
end Time_travel;

architecture Behavioral of Time_travel is

constant start_time : time := 10 ms; 
constant stop_time : time := 5 ms; 

procedure delay(t : time) is
begin
	wait for t;
end delay;

begin

McFly : process is
begin
        wait for start_time;
        report " Started " severity NOTE;        
	delay(stop_time - start_time);
--        wait for stop_time - start_time;
        report " Stopped " severity NOTE;   
        wait;
end process;
end Behavioral;
