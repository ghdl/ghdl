library ieee;
use ieee.std_logic_1164.all;

entity err_value06 is
  generic (v : string (1 to 5));
  port (a : std_logic;
        r : out std_logic);
end err_value06;

architecture behav of err_value06 is
  function is_ok(s : string) return boolean is
  begin
    return natural'value(s(10 to 12)) > 3;
  end;
begin
  r <= a when is_ok(v) else '0';
end behav;
