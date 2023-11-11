library ieee;
use ieee.std_logic_1164.all;

entity foobar is
  port (
    g1a, g1b : in  std_logic;
    g1q_n    : out std_logic
    );
end;

architecture ttl of foobar is
begin
  g1q_n <= not (g1a and g1b);
end;
