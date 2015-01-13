entity repro is
end;

architecture tb of repro is
  signal x : bit_vector(1 downto 0);
  signal y : bit;
begin
	assert (y = '1') = (x = "11");
end tb;

