library ieee;
use ieee.std_logic_1164.all;

entity arr01 is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of arr01 is
  type arr2d is array (integer range <>, integer range <>) of std_logic;
  signal s : arr2d(-1 to 0, 0 to 1);
begin
  s(-1, 0) <= b(0);
  s(-1, 1) <= b(1);
  s(0, 0) <= b(2);
  s(0, 1) <= b(3);

  s(0, 0) <= b (1);
  a <= not s(0, 1);
end behav;
