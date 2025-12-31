library ieee;
use ieee.std_logic_1164.all;

entity err_value01 is
  generic (p : string := "xx");
  port (a, b, c : in std_logic;
        r : out std_logic);
end err_value01;

architecture behav of err_value01 is
  constant res : boolean := character'value(p) > ' ';
begin
  r <= a and b when res else c;
end behav;
