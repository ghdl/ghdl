library ieee;
use ieee.std_logic_1164.all;

entity err_value03 is
  generic (p : string := "5 seconds");
  port (a, b, c : in std_logic;
        r : out std_logic);
end err_value03;

architecture behav of err_value03 is
  constant res : boolean := time'value(p) > 1 ns;
begin
  r <= a and b when res else c;
end behav;
