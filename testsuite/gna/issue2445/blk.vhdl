entity blk is
end;

architecture behav of blk is
  constant c : bit_vector := x"d";
begin
  b: block
    generic (TAPS: bit_vector);
    generic map (taps => c);
    subtype lfsr_t is bit_vector(TAPS'range);
  begin
    assert true;
  end block;
end behav;
