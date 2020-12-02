use work.test_pkg.all;

entity test is
	port (
		x : in number_t;
		y : in number_t;

		eq  : out boolean;
		neq : out boolean;
		lt  : out boolean;
		lte : out boolean;
		gt  : out boolean;
		gte : out boolean
	);
end entity;

architecture a of test is
begin
	eq  <= x  = y;
	neq <= x /= y;
	lt  <= x  < y;
	lte <= x <= y;
	gt  <= x  > y;
	gte <= x >= y;
end architecture;
