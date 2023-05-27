package lfsr is
	generic (TAPS: bit_vector);
	subtype lfsr_t is bit_vector(TAPS'range);
end;
