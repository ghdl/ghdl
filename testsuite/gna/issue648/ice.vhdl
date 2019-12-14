entity e is end entity;
library ieee;
architecture h of e is
  constant L :positive := integer(ieee.math_real.ceil(9.9));
  signal   s :bit_vector(31 downto 0);
  alias    a :bit_vector(L-1 downto 2) is s(L-1 downto 2);
  signal   b :bit_vector(L-1 downto 2);
begin
  a <= b;
end architecture;

