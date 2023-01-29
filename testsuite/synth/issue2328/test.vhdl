entity test is
end test;

architecture behavior of test is
signal a: bit_vector(3 downto 0);
signal b: bit;
begin
	b <= and a;
end behavior;
