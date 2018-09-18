entity repro is
end entity;

architecture h of repro is
  constant L :positive := 9 + now / 1 ns;
  signal   s :bit_vector(31 downto 0);
  alias    a :bit_vector(L-1 downto 2) is s(L-1 downto 2);
  signal   b :bit_vector(L-1 downto 2);
begin
  a <= b;
end architecture;

