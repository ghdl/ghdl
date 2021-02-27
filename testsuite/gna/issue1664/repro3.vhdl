entity repro3 is
end;

architecture behavioral of repro3 is
	signal s_index 	: natural;
	signal s_wrcnt	: integer range 0 to 3;
        constant c : integer_vector := (0 => 1);
begin
  assert (s_wrcnt = 3 and s_index => 5) = c;
end behavioral; -- behavioral
