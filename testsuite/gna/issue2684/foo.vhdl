library ieee;
use ieee.std_logic_1164.all;

entity foo is
end foo;

architecture arch of foo is
  signal a : std_logic;
  signal b : std_logic;
  attribute bar : boolean;
  attribute bar of b : signal is true; -- when commented out, the bug does not trigger
begin
  sequence seq is {a};
end arch;
