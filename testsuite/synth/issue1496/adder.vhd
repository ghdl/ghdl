library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity adder is
generic (N: integer := 4);
port( a,b : in  std_logic_vector(N-1 downto 0);
      cin : in  std_logic;
      sum : out std_logic_vector(N-1 downto 0);
      cout: out std_logic
      );
end;
architecture rtl2008 of adder is
begin
  (cout, sum) <= std_logic_vector(unsigned('0' & a) + unsigned('0' & b) + cin);
end;
