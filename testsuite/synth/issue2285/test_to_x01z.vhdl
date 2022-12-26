library ieee;
use ieee.std_logic_1164.all;

entity test_to_x01z is
  port (
    i_a : in  std_logic;
    i_b : in  std_logic;
    o_x : out std_logic
  );
end;

architecture rtl of test_to_x01z is
  signal a, b : std_logic;
begin
  a <= To_X01Z(i_a);
  b <= To_X01Z(i_b);
  o_x <= a and b;
end;
