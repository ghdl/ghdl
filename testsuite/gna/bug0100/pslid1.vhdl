library ieee;
use ieee.std_logic_1164.all;

entity pslid1 is
  port (a, b, ck : std_logic);
end;

architecture behav of pslid1 is
begin
  -- psl default clock is ck;
  -- psl my_assert_: assert always a -> b;
end behav;
