library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sub_adder is
  generic (
    l : natural := 4);
  port(A : in signed (l - 1 downto 0);
       B : in signed (l - 1 downto 0);
       Res : out signed (l - 1 downto 0));
end sub_adder;

architecture arith of sub_adder is
begin
  Res <= A + B;
end arith;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity adder is
  port( A : in signed (3 downto 0);
        B : in unsigned (3 downto 0);
        Result1 : out signed (3 downto 0);
        Result2 : out unsigned (3 downto 0));

  attribute my_attr : natural;
  attribute my_attr of a : signal is 12;
  attribute my_attr of adder : entity is 3;
end adder;

architecture arith of adder is
  signal s : signed(3 downto 0);
  attribute my_attr of s: signal is 2;
begin
  s <=  signed (B);
  inst_sub: entity work.sub_adder
    generic map (l => 4)
    port map (A, s, Result1);
  Result2 <= B + "11";
end arith;
