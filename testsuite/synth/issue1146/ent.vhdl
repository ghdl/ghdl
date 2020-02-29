entity ent is
	generic (
		t1 : time := 2 sec;
		t2 : time := 5 sec
	);
end;

architecture a of ent is
	constant t3 : time := t1 + t2;
	constant diff : time := abs (t1 - t2);

	constant shorter : time := minimum(t1, t2);
	constant longer  : time := maximum(t1, t2);

	constant ratio1 : natural := t1 / t2;
	constant ratio2 : natural := (t1 / 2) / (t2 * 0.5);
	constant ratio3 : natural := (t1 * 2) / (t2 / 0.5);
begin
	assert t3 > 6 sec;
	assert t3 = 7 sec;
	assert t3 < 8 sec;

	assert t3 /= 1 ns;

	assert t3 >= shorter;
	assert shorter <= longer;

	assert diff = longer - shorter;
	assert -diff = +(shorter-longer);

	assert ratio1 = ratio2;
	assert ratio1 = ratio3;

	assert t1 * 2 = 2 * t1;
	assert t1 * 0.5 = 0.5 * t1;
end;
