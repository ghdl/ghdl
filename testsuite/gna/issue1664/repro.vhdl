entity repro is
end;

architecture behavioral of repro is
	signal s_index 	: natural;
	signal s_wrcnt	: integer range 0 to 3;
begin
	state_machine : process
        begin
          assert (s_wrcnt = 3 and s_index => 5);
	end process state_machine;
end behavioral; -- behavioral
