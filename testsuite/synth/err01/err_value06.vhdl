library ieee;
use ieee.std_logic_1164.all;

entity err_value06 is
  port (a : string(1 to 3);
        r : out natural);
end err_value06;

architecture behav of err_value06 is
begin
  r <= natural'value(a);
end behav;
