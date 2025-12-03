library ieee;
use ieee.std_logic_1164.all;

entity img01 is
end;

architecture arch of img01 is
  type my_enum is (e1, e2, 'z', \ext\);
begin
  process
    variable v : my_enum;
  begin
    v := e1;
    assert my_enum'image(v) = "e1" severity failure;

    v := 'z';
    assert my_enum'image(v) = "'z'" severity failure;

    v := \ext\;
    assert my_enum'image(v) = "\ext\" severity failure;

    wait;
  end process;
end;
