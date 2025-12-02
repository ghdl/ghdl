library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity adder is
  port( A : in signed (3 downto 0);
        B : in unsigned (3 downto 0);
        Result1 : out signed (3 downto 0);
        Result2 : out unsigned (3 downto 0));
end adder;

architecture arith of adder is
begin
  Result1 <= A + "11";
  Result2 <= B + "11";
end arith;
