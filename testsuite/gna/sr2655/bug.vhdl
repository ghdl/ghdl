library ieee;
use ieee.std_logic_1164.all;

entity bug is
end;

architecture this of bug is
  function f return integer is
    constant v: std_logic_vector := std_logic_vector'("01HLZX-U");
  begin
    assert false report "case 2 starts" severity note;
    for i in v'range loop
      assert false report "case 2: "&integer'image(i) severity note;
    end loop;
    return 0;
  end;
begin
  process
    constant v: std_logic_vector := std_logic_vector'("01HLZX-U");
    variable a: integer;
  begin
    assert false report "case 1 starts" severity note;
    for i in v'range loop
      assert false report "case 1: "&integer'image(i) severity note;
    end loop;
    a := f;
    wait;
  end process;
end;
