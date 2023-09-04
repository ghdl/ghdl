library ieee;
use ieee.std_logic_1164.all;

package bug is
  function foo (a, b: std_ulogic) return std_ulogic;
end package;

package body bug is

function foo (a, b: std_ulogic) return std_ulogic is
  variable res: std_ulogic;
begin
  with a&b select
    res := '0' when "0U"|"U0"|"11",
           '1' when "U1"|"1U"|"00",
           'U' when others;
  return res;
end function;

end package body;
