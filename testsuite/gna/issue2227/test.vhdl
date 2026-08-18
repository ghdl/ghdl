library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end test;

architecture rtl of test is

  signal a : character;
  signal b : character;
  signal c : character;
  signal k : string(1 to 5);
begin
  process
    variable  y : string(1 to 8);
  begin
    y := "abcdefgh";
    (1 => a, 2 => b, 3 => c, 4 to 8 => k) <= y;
    wait for 1 ns;
    report a & " " & b & " " & c &  " " & k;
    report a & b & c & k;
    wait;
  end process;
end rtl;
