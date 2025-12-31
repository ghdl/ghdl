library ieee;
use ieee.std_logic_1164.all;

entity err_value02 is
  generic (p : string := "xx");
  port (a, b, c : in std_logic;
        r : out std_logic);
end err_value02;

architecture behav of err_value02 is
  constant res : boolean := real'value(p) > 1.0;
begin
  r <= a and b when res else c;
end behav;
