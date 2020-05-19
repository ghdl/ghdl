entity st is
end;

architecture behav of st is
  shared variable cnt : natural := 2;

  impure function f return natural is
  begin
    cnt := cnt + 1;
    return cnt;
  end f;

  signal s1, s2, s3 : bit_vector (1 to f) := (others => '0');
begin
  assert false report "s1'length=" & natural'image (s1'length);
  assert false report "s2'length=" & natural'image (s2'length);
  assert false report "s3'length=" & natural'image (s3'length);
end behav;
