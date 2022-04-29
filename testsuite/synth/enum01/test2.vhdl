use work.test_pkg.all;

entity test2 is
	port (
		eq  : out boolean;
		neq : out boolean;
		lt  : out boolean;
		lte : out boolean;
		gt  : out boolean;
		gte : out boolean
	);
end entity;

architecture a of test2 is
  function is_eq (l, r : number_t) return boolean is
  begin
    return l = r;
  end is_eq;

  function is_ne (l, r : number_t) return boolean is
  begin
    return l /= r;
  end is_ne;

  function is_lt (l, r : number_t) return boolean is
  begin
    return l < r;
  end is_lt;

  function is_le (l, r : number_t) return boolean is
  begin
    return l <= r;
  end is_le;

  function is_gt (l, r : number_t) return boolean is
  begin
    return l > r;
  end is_gt;

  function is_ge (l, r : number_t) return boolean is
  begin
    return l >= r;
  end is_ge;

  constant x : number_t := ONE;
  constant y : number_t := THREE;
begin
	eq  <= is_eq (x, y);
	neq <= is_ne (x, y);
	lt  <= is_lt (x, y);
	lte <= is_le (x, y);
	gt  <= is_gt (x, y);
	gte <= is_ge (x, y);
end architecture;
