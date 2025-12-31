library ieee;
use ieee.std_logic_1164.all;

entity err_image01 is
  generic (v : string (1 to 5));
  port (a : std_logic;
        r : out std_logic);
end err_image01;

architecture behav of err_image01 is
  type int_array is array (natural range <>) of integer;
  
  function is_ok(s : int_array) return boolean is
  begin
    return integer'image(s(0)) = "'A'";
  end;
  constant c : int_array (1 to 3) := (1,2,3);
begin
  r <= a when is_ok(c) else '0';
end behav;
