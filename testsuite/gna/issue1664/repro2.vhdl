entity repro2 is
end;

architecture behavioral of repro2 is
	signal s_index 	: natural;
	signal s_wrcnt	: integer range 0 to 3;
begin
  assert (s_wrcnt = 3 and s_index => 5);
end behavioral; -- behavioral
