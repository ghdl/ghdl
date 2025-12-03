library ieee;
use ieee.std_logic_1164.all;

entity img02 is
end;

architecture arch of img02 is
  type my_enum is (e1, e2, 'z', \eXt\, \long\\ext\);
begin
  process
    variable v : my_enum;
  begin
    v := e1;
    assert to_string(v) = "e1" severity failure;

    v := 'z';
    assert to_string(v) = "z" severity failure;

    v := \eXt\;
    assert to_string(v) = "eXt" severity failure;

    v := \long\\ext\;
    assert to_string(v) = "long\ext" severity failure;

    wait;
  end process;
end;
