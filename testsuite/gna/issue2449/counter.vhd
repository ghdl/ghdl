package lfsr is
	generic (TAPS: bit_vector);
	subtype lfsr_t is bit_vector(TAPS'range);
end;

entity counter is
end;

architecture rtl of counter is
	package pkg is new work.lfsr generic map (TAPS => 10D"0");
	signal c: pkg.lfsr_t;
begin
end;
