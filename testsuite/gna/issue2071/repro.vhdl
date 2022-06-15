library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
end;

architecture behav of repro is
  type matrixType is array(natural range <>) of std_logic_vector;
  signal matrix : matrixType(0 to 15)(7 downto 0);

  -- Missing feature:
  signal row1 : unsigned(matrix'element'range);

  -- As a workaround:
  signal row2 : unsigned(matrix(matrix'low)'range);
begin
end behav;
